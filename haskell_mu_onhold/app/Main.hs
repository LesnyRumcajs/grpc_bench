{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import Mu.Quasi.GRpc
import Mu.Server (MonadServer, SingleServerT, singleService, method)
import Mu.Schema ((:/:), Term)
import Mu.Schema.Optics (record1, (^.))
import Mu.GRpc.Server (runGRpcApp, msgProtoBuf)

grpc "HelloWorld" id "helloworld.proto"

main :: IO ()
main = runGRpcApp msgProtoBuf 50051 greeterServer

type HelloRequest = Term HelloWorld (HelloWorld :/: "HelloRequest")
type HelloReply = Term HelloWorld (HelloWorld :/: "HelloReply")

sayHello :: MonadServer m => HelloRequest -> m HelloReply
sayHello req = pure $ record1 (req ^. #name)

greeterServer :: MonadServer m => SingleServerT info Greeter  m _
greeterServer = singleService (method @"SayHello" sayHello)
