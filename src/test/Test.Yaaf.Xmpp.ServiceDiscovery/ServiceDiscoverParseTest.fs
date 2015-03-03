// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
open System.IO
open NUnit.Framework
open FsUnit
open Test.Yaaf.Xmpp.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Stream
open System.Threading.Tasks
open Yaaf.IO
open Yaaf.TestHelper
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.ServiceDiscovery


[<TestFixture>]
type ``Test-Yaaf-Xmpp-ServiceDiscovery-Parsing: Test that parsing works``() as this = 
    inherit XmlStanzaParsingTestClass()
    
    let discoTest stanzaString (info : IStanza) (elem : ServiceDiscoveryAction) = 
        let newStanza = Parsing.createDiscoElement info.Header.Id.Value info.Header.From.Value info.Header.To.Value elem
        this.GenericTest Parsing.discoContentGenerator stanzaString newStanza
    
    [<Test>]
    member this.``Check that we can parse empty disco info element``() = 
        let stanza = "<iq type='get'
    from='romeo@montague.net/orchard'
    to='plays.shakespeare.lit'
    id='info1'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>"
        let info = this.Test stanza
        Parsing.isContentDisco info |> should be True
        let elem = Parsing.parseContentDisco info
        elem |> should be (equal <| ServiceDiscoveryAction.Discover(DiscoverType.Info, None))
        discoTest stanza info elem
    
    [<Test>]
    member this.``Check that we can parse empty disco item element``() = 
        let stanza = "<iq type='get'
    from='romeo@montague.net/orchard'
    to='shakespeare.lit'
    id='items1'>
  <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>"
        let info = this.Test stanza
        Parsing.isContentDisco info |> should be True
        let elem = Parsing.parseContentDisco info
        elem |> should be (equal <| ServiceDiscoveryAction.Discover(DiscoverType.Items, None))
        discoTest stanza info elem