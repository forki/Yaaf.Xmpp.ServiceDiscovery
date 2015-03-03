// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
(*
open FsUnit
open NUnit.Framework
open Test.Yaaf.Xmpp
open Yaaf.Xmpp.Features
open Yaaf.Xmpp
open Yaaf.Xmpp.Negotiation
open Yaaf.Xmpp.Handler
open Yaaf.Helper
open Yaaf.TestHelper
open Yaaf.Xmpp.IM.MessageArchiving.Core
open Yaaf.Xmpp.IM.MessageArchiving.Data
open Yaaf.Xmpp.IM.Data
open Yaaf.Xmpp.IM.Core
open Yaaf.Xmpp.IM.Server
open Yaaf.Xmpp.ServiceDiscovery.Core

[<TestFixture>]
type XmppServiceDiscoveryTestClass() =
    inherit XmppIMServerTestClass()

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

        *)