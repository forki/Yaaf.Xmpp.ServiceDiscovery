// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.ServiceDiscovery

open Yaaf.Xmpp
open Yaaf.Xmpp.XmlStanzas

type DiscoverType = 
    | Info
    | Items
type IdentityItemInfo =
  { Name : string option
    Category : string
    Type : string }
type FeatureItemInfo = 
  { Var : string }
type NodePath = string option
type DiscoItem = 
  { Jid : JabberId
    Name : string option
    Node : string option}
type InfoResult = IdentityItemInfo list * FeatureItemInfo list 
type ItemsResult = DiscoItem list

type ServiceDiscoveryAction = 
    | Discover of DiscoverType * NodePath
    | InfoResult of InfoResult * NodePath
    | ItemsResult of ItemsResult * NodePath with 
    member x.DiscoType = 
        match x with 
        | Discover(x, _) -> x
        | InfoResult _ -> DiscoverType.Info
        | ItemsResult _ -> DiscoverType.Items
    member x.NodePath = 
        match x with 
        | Discover(_, y) 
        | InfoResult (_,y) 
        | ItemsResult (_,y) -> y


type ServiceDiscoveryStanza = Stanza<ServiceDiscoveryAction>

module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing

    let discoNs = "http://jabber.org/protocol/disco"
    let discoSubNs n = sprintf "%s#%s" discoNs n
    let discoItemsNs = discoSubNs "items"
    let discoInfoNs = discoSubNs "info"
    let isContentDisco (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind (fun e -> e.Name = getXName "query" discoItemsNs || e.Name = getXName "query" discoInfoNs)
        queryItem.IsSome //|| (stanza.Contents.Children |> List.isEmpty && stanza.Header.Type.IsSome && stanza.Header.Type.Value = "result")
    
    let parseContentDiscoItem (elem:XElement) = 
        { Jid = 
            match elem |> tryXAttrValue (getXName "jid" "") with
            | Some e -> JabberId.Parse e
            | None -> failwith "disco item without jid attribute is not expected"
          Name = elem |> tryXAttrValue (getXName "name" "")
          Node = elem |> tryXAttrValue (getXName "node" "") }
            
    let parseContentIdentityItem (elem:XElement) = 
        { Category = 
            match elem |> tryXAttrValue (getXName "category" "") with
            | Some e -> e
            | None -> failwith "identity must have category attribute"
          Name = elem |> tryXAttrValue (getXName "name" "")
          Type = 
            match elem |> tryXAttrValue (getXName "type" "") with
            | Some e -> e
            | None -> failwith "identity must have type attribute" }

    let parseContentFeatureItem (elem:XElement) = 
        { Var = 
            match elem |> tryXAttrValue (getXName "var" "") with
            | Some e -> e
            | None -> failwith "feature disco element without var attribute is not expected" }
    let parseContentDisco (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind (fun e -> e.Name = getXName "query" discoItemsNs)
        let queryItem, t =
            match queryItem with
            | Some query ->
                query, DiscoverType.Items
            | None ->
                stanza.Contents.Children 
                |> Seq.filter (fun e -> e.Name = getXName "query" discoInfoNs)
                |> Seq.exactlyOne,
                DiscoverType.Info

        let nodeAttribute =
            queryItem |> tryXAttrValue (getXName "node" "")

        match stanza.Header.Type.Value with
        | "get" -> 
            assert (queryItem.Elements() |> Seq.isEmpty)
            ServiceDiscoveryAction.Discover (t, nodeAttribute)
        | "result" ->
            match t with
            | DiscoverType.Items ->
                let itemList = 
                    queryItem.Elements()
                    |> Seq.filter (fun e -> e.Name = getXName "item" discoItemsNs)
                    |> Seq.map (parseContentDiscoItem)
                    |> Seq.toList
                ServiceDiscoveryAction.ItemsResult (itemList, nodeAttribute)
            | DiscoverType.Info ->
                let identityList = 
                    queryItem.Elements()
                    |> Seq.filter (fun e -> e.Name = getXName "identity" discoInfoNs)
                    |> Seq.map (parseContentIdentityItem)
                    |> Seq.toList
                let featureList = 
                    queryItem.Elements()
                    |> Seq.filter (fun e -> e.Name = getXName "feature" discoInfoNs)
                    |> Seq.map (parseContentFeatureItem)
                    |> Seq.toList
                ServiceDiscoveryAction.InfoResult((identityList, featureList), nodeAttribute)
        | _ -> failwithf "unknown type in disco stanza: %s" stanza.Header.Type.Value

    let parseDiscoStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentDisco) elem
    let createDiscoItemElement (item: DiscoItem) = 
        [
            yield XAttribute(getXName "jid" "", item.Jid.FullId) :> obj
            if item.Node.IsSome then
                yield XAttribute(getXName "node" "", item.Name.Value) :> obj
            if item.Name.IsSome then
                yield XAttribute(getXName "name" "", item.Name.Value) :> obj
        ] |> getXElemWithChilds (getXName "item" discoItemsNs)
    let createIdentityItemElement (item: IdentityItemInfo) = 
        [
            yield XAttribute(getXName "category" "", item.Category) :> obj
            if item.Name.IsSome then
                yield XAttribute(getXName "name" "", item.Name.Value) :> obj
            yield XAttribute(getXName "type" "", item.Type) :> obj
        ] |> getXElemWithChilds (getXName "identity" discoInfoNs)
        
    let createFeatureItemElement (item: FeatureItemInfo) = 
        [
            yield XAttribute(getXName "var" "", item.Var) :> obj
        ] |> getXElemWithChilds (getXName "feature" discoInfoNs)
        
    let createDiscoStanzaElement (content:ServiceDiscoveryAction) = 
        let nodePath = content.NodePath 
        let t = content.DiscoType
        let discoNs = 
            match t with
            | DiscoverType.Items -> discoItemsNs
            | DiscoverType.Info -> discoInfoNs
        [
            if nodePath.IsSome then
                yield XAttribute(getXName "node" "", nodePath.Value) :> obj
            match content with
            | ServiceDiscoveryAction.Discover _ -> ()
            | ServiceDiscoveryAction.ItemsResult(items, _) -> 
                yield! items |> Seq.map createDiscoItemElement |> Seq.cast
            | ServiceDiscoveryAction.InfoResult((identities, features), _) -> 
                yield! identities |> Seq.map createIdentityItemElement |> Seq.cast
                yield! features |> Seq.map createFeatureItemElement |> Seq.cast
        ] |> getXElemWithChilds (getXName "query" discoNs)

    let discoContentGenerator = ContentGenerator.SimpleGenerator createDiscoStanzaElement
    let createDiscoElement (id:string) (source:JabberId) (target:JabberId) (data:ServiceDiscoveryAction) = 
        let cType = 
            match data with
            | ServiceDiscoveryAction.Discover _ -> "get"
            | ServiceDiscoveryAction.InfoResult _
            | ServiceDiscoveryAction.ItemsResult _ -> "result"
        Stanza<_>.CreateGen discoContentGenerator
          { To = Some target
            From = Some source
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data