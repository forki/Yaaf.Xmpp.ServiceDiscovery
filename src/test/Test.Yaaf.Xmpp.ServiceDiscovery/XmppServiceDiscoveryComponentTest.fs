// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
(*
open FsUnit
open NUnit.Framework
open Yaaf.Helper
open Yaaf.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp.Features
open Yaaf.Xmpp
open Yaaf.Xmpp.Negotiation
open Yaaf.Xmpp.Handler
open Yaaf.Xmpp.IM.MessageArchiving.Core
open Yaaf.Xmpp.IM.Data
open Yaaf.Xmpp.IM.Core
open Yaaf.Xmpp.IM.Server

[<TestFixture>]
type XmppServiceDiscoveryComponentTestClass() =
    inherit XmppIMServerComponentTestClass()
    
    override x.ServerConfig streamType = 
        let baseFun =  base.ServerConfig streamType
        fun config ->
            let config = baseFun config
            config.AddPlugin Yaaf.Xmpp.ServiceDiscovery.Core.DiscoPlugin
            config
    override x.ClientConfig instanceNum = 
        let baseFun =  base.ClientConfig instanceNum
        fun config ->
            let config = baseFun config
            config.AddPlugin Yaaf.Xmpp.ServiceDiscovery.Core.DiscoPlugin
            config
    
    override x.Setup () = 
        base.Setup()

    override x.TearDown() = 
        base.TearDown()
                
    [<Test>]
    member x.``Check if we can send a disco request from component to client``() =
        let xmppClient_0 =  x.XmppClients 0
        let xmppClient_1 =  x.XmppClients 1
        
        let receiver_0 = NUnitHelper.StanzaReceiver xmppClient_0
        let result_0 =  receiver_0 |> NUnitHelper.readNext |> Async.StartAsTask
        
        let receiver_1 = NUnitHelper.StanzaReceiver xmppClient_1
        let result_1 = receiver_1 |> NUnitHelper.readNext |> Async.StartAsTask
        // Wait for negotiation events, so that server can actually deliver the message
        xmppClient_0.WriteRaw "<finishnegotiation/>" |> x.WaitProcess // force end of negotiation procedure
        x.ServerNegotiatedClient 0 |> waitTaskI "client 0 connected to server"
        
        xmppClient_1
            .WriteRaw (sprintf "<iq to='%s' from='comp0.nunit.org' id='disco_1' type='result'>
  <query xmlns='http://jabber.org/protocol/disco#items' /></iq>" xmppClient_0.LocalJid.FullId) |> x.WaitProcess
        x.ServerNegotiatedClient 1 |> waitTaskI "client 1 connected to server"

        let stanza, receiver = result_0 |> waitTaskI "result"
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        stanza.Header.Type.IsSome |> should be True
        stanza.Header.Type.Value |> should be (equal "result")
        stanza.Contents.Children.Length |> should be (equal 1)
                
    [<Test>]
    member x.``Check if we can request resources``() =
        let xmppClient_0 =  x.XmppClients 0
        let xmppClient_1 =  x.XmppClients 1
        
        let receiver_0 = NUnitHelper.StanzaReceiver xmppClient_0
        let result_0 =  receiver_0 |> NUnitHelper.readNext |> Async.StartAsTask
        
        let receiver_1 = NUnitHelper.StanzaReceiver xmppClient_1
        let result_1 = receiver_1 |> NUnitHelper.readNext |> Async.StartAsTask
        // Wait for negotiation events, so that server can actually deliver the message
        xmppClient_0.WriteRaw "<finishnegotiation/>" |> x.WaitProcess // force end of negotiation procedure
        x.ServerNegotiatedClient 0 |> waitTaskI "client 0 connected to server"
        
        xmppClient_1
            .WriteRaw (sprintf "<iq to='%s' from='comp0.nunit.org' id='disco_1' type='result'>
  <query xmlns='http://jabber.org/protocol/disco#items' /></iq>" xmppClient_0.LocalJid.FullId) |> x.WaitProcess
        x.ServerNegotiatedClient 1 |> waitTaskI "client 1 connected to server"

        let stanza, receiver = result_0 |> waitTaskI "result"
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        stanza.Header.Type.IsSome |> should be True
        stanza.Header.Type.Value |> should be (equal "result")
        stanza.Contents.Children.Length |> should be (equal 1)


*)