# whisper

[![Package Version](https://img.shields.io/hexpm/v/whisper)](https://hex.pm/packages/whisper)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/whisper/)

A lightweight, cross-platform pub/sub library for Gleam with topic-based messaging and buffered subscriptions.

## Features

- 📢 **Topic-based messaging** - Organize messages by topics
- 🔄 **Buffered subscriptions** - Pull messages on demand with configurable buffer capacity
- ⚡ **Callback listeners** - Real-time push notifications
- 🌐 **Cross-platform** - Works on both Erlang and JavaScript targets
- 🎯 **Type-safe** - Leverages Gleam's type system for safety

## Installation

Add whisper to your Gleam project:

```sh
gleam add whisper
```

## Usage

### Creating a Whisper Instance

```gleam
let whisper = whisper.new(capacity: 100)
```

The capacity parameter determines how many messages each subscription can buffer before older messages are dropped.

### Subscriptions (Pull-based)

Subscriptions provide a buffered queue where messages are stored until you retrieve them:

```gleam
let sub = whisper.subscribe(whisper, "events")

// Publish some messages
whisper.publish(whisper, "events", 1)
whisper.publish(whisper, "events", 2)
whisper.publish(whisper, "events", 3)

// Pull messages on demand
let assert Ok(1) = sub.receive()
let assert Ok(2) = sub.receive()
let assert Ok(3) = sub.receive()

sub.cancel()
```

### Callbacks (Push-based)

Register callback listeners for real-time notifications:

```gleam
let cancel = whisper.on(whisper, "notifications", fn(msg) {
  io.println("Received: " <> msg)
})

whisper.publish(whisper, "notifications", "Hello!")

cancel()
```

### Multiple Subscribers

Multiple subscribers can listen to the same topic:

```gleam
let sub1 = whisper.subscribe(whisper, "news")
let sub2 = whisper.subscribe(whisper, "news")

whisper.publish(whisper, "news", "Breaking news!")

// Both subscribers receive the message
let assert Ok(msg1) = sub1.receive()
let assert Ok(msg2) = sub2.receive()
```
