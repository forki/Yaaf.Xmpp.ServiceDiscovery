// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------

(*
    Implementation of the Xmpp0030 Service Discovery Specification (http://xmpp.org/extensions/xep-0030.html).
    This implementation provides the required api for other apis/plugins to register features.
    It will also respond to dico requests.
*)
namespace Yaaf.Xmpp.ServiceDiscovery

open Yaaf.DependencyInjection

open Yaaf.Helper
open Yaaf.Logging

open Yaaf.Xmpp
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server

type IDiscoService = 
    abstract RegisterDiscoItem : NodePath * DiscoItem -> unit
    abstract RegisterIdentityItem : NodePath * IdentityItemInfo -> unit
    abstract RegisterFeatureItem : NodePath * FeatureItemInfo -> unit
    abstract RequestDiscoInfos : JabberId * NodePath -> Async<InfoResult * NodePath>
    abstract RequestDiscoItems : JabberId * NodePath -> Async<ItemsResult * NodePath>

type DiscoPlugin 
    (runtimeConfig : IRuntimeConfig, neg:INegotiationService, stanzas:IXmlStanzaService, kernel : IKernel,
     registrar : IPluginManagerRegistrar, addressing : IAddressingService) =

    let discoInfoFeatures = new System.Collections.Generic.Dictionary<_,_>()
    let discoItemsFeatures = new System.Collections.Generic.Dictionary<_,_>()

    let getKey (node:NodePath) =
        match node with
        | None -> "/"
        | Some s -> sprintf "/%s" s

    let addressedStanzaReceived (stanza:Stanza) = 
        // if the stanza is addressed to us then respond to disco request
        if Parsing.isContentDisco stanza then
            Log.Verb (fun () -> L "handling disco element (DiscoPlugin)")
            let msgStanza = Stanza<_>.Create(stanza, Parsing.parseContentDisco stanza)
            let sendResult d = 
                let stanza = Parsing.createDiscoElement (stanza.Header.Id.Value) neg.LocalJid neg.RemoteJid (d)
                stanzas.QueueStanzaGeneric None stanza
            let isServerBareIdStanza =
                runtimeConfig.IsServerSide && stanza.Header.To.IsSome && stanza.Header.To.Value.Localpart.IsSome
            match msgStanza.Data with
            | ServiceDiscoveryAction.Discover(discoType, node) ->
                let key = getKey node
                let tryGet def key (dic:System.Collections.Generic.IDictionary<_,_>)= 
                    match dic.TryGetValue key with
                    | false, _ -> def
                    | true, v -> v
                match discoType with
                | DiscoverType.Info ->
                    if isServerBareIdStanza then
                        //let api = kernel.Get<IServerApi>()
                        // TODO: check if user is registered!
                        sendResult (ServiceDiscoveryAction.InfoResult(([{ Name = None; Category = "account"; Type = "registered" }], []), node))
                    else
                        sendResult (ServiceDiscoveryAction.InfoResult(tryGet ([],[]) key discoInfoFeatures, node))
                | DiscoverType.Items ->
                    if isServerBareIdStanza then
                        // TODO: return available resources (not connected ones!)
                        let api = kernel.Get<IServerApi>()
                        sendResult 
                            (ServiceDiscoveryAction.ItemsResult(
                                api.ConnectionManager.GetConnections(stanza.Header.To.Value)
                                |> Seq.map (fun client -> { Jid = client.RemoteJid; Name = None; Node = None})
                                |> Seq.toList, 
                                node))
                    else
                        let registeredItems = tryGet [] key discoItemsFeatures
                        let components =
                            if runtimeConfig.IsServerSide then
                                let api = kernel.Get<IServerApi>()
                                let components = api.ConnectedComponents
                                components |> List.map (fun comp -> { Jid = comp.Jid; Name = comp.Name; Node = None})
                            else []
                        sendResult (ServiceDiscoveryAction.ItemsResult(registeredItems @ components, node))
            | _ ->
                ()
    let requestDisco (target:JabberId, discoType:DiscoverType, node:NodePath) =
        async {
            let request = ServiceDiscoveryAction.Discover (discoType, node)
            let stanza = Parsing.createDiscoElement (stanzas.GenerateNextId()) neg.LocalJid target request
            let! result = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return Parsing.parseContentDisco result
        }
        |> Log.TraceMe


    let requestDiscoItems (target:JabberId, node:NodePath) =
        async {
            let! action = requestDisco (target, DiscoverType.Items, node)
            match action with
            | ServiceDiscoveryAction.ItemsResult (items, data) ->
                return items, data
            | _ -> 
                return failwithf "unexpected ServiceDiscoveryAction at this point: %A" action
        }
        |> Log.TraceMe
    let requestDiscoInfo (target:JabberId, node:NodePath) =
        async {
            let! action = requestDisco (target, DiscoverType.Info, node)
            match action with
            | ServiceDiscoveryAction.InfoResult (info, data) ->
                return info, data
            | _ -> 
                return failwithf "unexpected ServiceDiscoveryAction at this point: %A" action
        } 
        |> Log.TraceMe
        
    let registerDiscoItem (node:NodePath, discoItem:DiscoItem) =
        let key = getKey node
        match discoItemsFeatures.TryGetValue key with
        | true, v ->  discoItemsFeatures.[key] <- discoItem :: v
        | false, _ -> discoItemsFeatures.Add(key, [ discoItem ])

    let registerIdentityItem (node:NodePath, identityItem:IdentityItemInfo) =
        let key = getKey node
        match discoInfoFeatures.TryGetValue key with
        | true, (identities,features) ->  discoInfoFeatures.[key] <- (identityItem :: identities, features)
        | false, _ -> discoInfoFeatures.Add(key, ([ identityItem ], []))

    let registerFeatureItem (node:NodePath, featureItem:FeatureItemInfo) =
        let key = getKey node
        match discoInfoFeatures.TryGetValue key with
        | true, (identities,features) -> discoInfoFeatures.[key] <- (identities, featureItem :: features)
        | false, _ -> discoInfoFeatures.Add(key, ([], [ featureItem ]))


    let addressing =
        { new IRawStanzaPlugin with        
            member __.ReceivePipeline = 
                { Pipeline.empty "ServiceDiscoveryPipeline" with
                    HandlerState =
                        fun info ->
                            let stanza = info.Result.Element
                            if Parsing.isContentDisco stanza && addressing.IsLocalStanzaMaybeServer stanza then HandlerState.ExecuteAndHandle
                            else HandlerState.Unhandled
                    Process =
                        fun info ->
                            async {
                                let elem = info.Result.Element
                                addressedStanzaReceived (elem)
                            } |> Async.StartAsTaskImmediate
                } :> IPipeline<_> }
    do
        registrar.RegisterFor<IRawStanzaPlugin> addressing

    interface IDiscoService with
        member __.RegisterDiscoItem (n, i) = registerDiscoItem (n, i)
        member __.RegisterIdentityItem (n, i) = registerIdentityItem (n, i)
        member __.RegisterFeatureItem (n, i) = registerFeatureItem (n, i)
        member __.RequestDiscoInfos (jid, path) = requestDiscoInfo (jid, path)
        member __.RequestDiscoItems (jid, path) = requestDiscoItems (jid, path)

    interface IXmppPlugin with
        member x.PluginService = Service.FromInstance<IDiscoService,_> x
        member __.Name = "DiscoPlugin"

