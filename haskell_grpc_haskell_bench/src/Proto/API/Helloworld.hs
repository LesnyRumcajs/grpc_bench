{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Helloworld (
    module Proto.Helloworld
  ) where

import Data.ProtoLens.Labels ()

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Helloworld

type instance RequestMetadata          (Protobuf Greeter meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Greeter meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Greeter meth) = NoMetadata
