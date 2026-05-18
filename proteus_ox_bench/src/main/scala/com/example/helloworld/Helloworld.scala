package com.example.helloworld

import proteus.*
import proteus.Modifiers.*
import proteus.Modifiers.OneOfFlag.Inline
import zio.blocks.schema.Schema

given ProtobufDeriver = ProtobufDeriver
  .modifier[Choice](oneOf(Inline))
  .modifier[Pet](nested)
  .modifier[Color](nested)

enum Choice {
  case C1(value: String)
  case C2(value: Boolean)
}

object Choice {
  // Schemas that encode the oneof cases as scalar primitives on the wire,
  // so the rendered proto matches `oneof choice { string c1 = 7; bool c2 = 8; }`.
  given Schema[Choice.C1] = Schema[String].transform[Choice.C1](Choice.C1(_), _.value)
  given Schema[Choice.C2] = Schema[Boolean].transform[Choice.C2](Choice.C2(_), _.value)
}

enum Color {
  case Black, White, Blue, Red, Yellow, Green
}

case class Pet(name: String, color: Color)

case class Hello(
  name: String,
  d: Double,
  f: Float,
  b: Boolean,
  n: Int,
  l: Long,
  choice: Choice,
  pets: List[Pet]
)

case class HelloRequest(request: Hello) derives ProtobufCodec
case class HelloReply(response: Hello) derives ProtobufCodec

val sayHelloRpc    = Rpc.unary[HelloRequest, HelloReply]("SayHello")
val greeterService = Service("helloworld", "Greeter").rpc(sayHelloRpc)
