# whisper

[![Package Version](https://img.shields.io/hexpm/v/whisper)](https://hex.pm/packages/whisper)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/whisper/)
![Status: Experimental](https://img.shields.io/badge/status-experimental-orange)
![Stability: Unstable](https://img.shields.io/badge/stability-unstable-red)

> ⚠️ **Warning:** This project is experimental and unstable. APIs may change significantly between versions.

A lightweight, cross-platform pub/sub library for Gleam with topic-based messaging and buffered subscriptions.

Whisper provides process-local pub/sub with configurable message buffers, supporting both streaming subscriptions and callback-based handlers.

## Features

- 📢 **Topic-based messaging** - Organize messages by named topics
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

### Creating a Topic

```gleam
import whisper

let assert Ok(events) = whisper.new_topic("events", capacity: 100)
```

The capacity parameter determines how many messages each subscription can buffer before older messages are dropped.

### Subscriptions (Pull-based)

Subscriptions provide a buffered queue where messages are stored until you retrieve them:

```gleam
let sub = whisper.subscribe(events)

// Publish some messages
whisper.publish(events, 1)
whisper.publish(events, 2)
whisper.publish(events, 3)

// Pull messages on demand
let assert whisper.Message(1) = sub.receive()
let assert whisper.Message(2) = sub.receive()
let assert whisper.Message(3) = sub.receive()

// Check for messages without blocking
case sub.receive() {
  whisper.Message(msg) -> io.println("Got message")
  whisper.Empty -> io.println("No messages available")
  whisper.Closed(err) -> io.println("Topic closed")
}

sub.cancel()
```

### Callbacks (Push-based)

Register callback listeners for real-time notifications:

```gleam
let assert Ok(notifications) = whisper.new_topic("notifications", capacity: 50)

let unsubscribe = whisper.on(notifications, fn(msg) {
  io.println("Received: " <> msg)
})

whisper.publish(notifications, "Hello!")

unsubscribe()
```

### Multiple Subscribers

Multiple subscribers can listen to the same topic:

```gleam
let assert Ok(news) = whisper.new_topic("news", capacity: 100)

let sub1 = whisper.subscribe(news)
let sub2 = whisper.subscribe(news)

whisper.publish(news, "Breaking news!")

// Both subscribers receive the message
let assert whisper.Message(msg1) = sub1.receive()
let assert whisper.Message(msg2) = sub2.receive()
```

### Closing Topics

Close a topic to stop all subscriptions and prevent new messages:

```gleam
whisper.close(events)

// Subscribers will receive Closed error
case sub.receive() {
  whisper.Closed(whisper.TopicIsNotRegistered) -> {
    io.println("Topic has been closed")
  }
  _ -> Nil
}
```
