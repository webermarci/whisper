/// A subscription to a topic that allows pulling messages on demand.
///
/// Use `receive()` to pull the next message from the buffer (non-blocking),
/// and `cancel()` to unsubscribe and clean up resources.
pub type Subscription(a) {
  Subscription(receive: fn() -> Result(a, Nil), cancel: fn() -> Nil)
}

/// An opaque type representing a pub/sub whisper instance.
pub type Whisper(a)

/// Creates a new whisper pub/sub system with a specified buffer capacity.
///
/// The capacity determines how many messages each subscription can buffer
/// before older messages are dropped.
///
/// ## Example
///
/// ```gleam
/// let whisper = whisper.new(100)
/// ```
pub fn new(capacity: Int) -> Whisper(a) {
  platform_new(capacity)
}

@external(erlang, "whisper_ffi", "new")
@external(javascript, "./whisper_ffi.mjs", "new_whisper")
fn platform_new(capacity: Int) -> Whisper(a)

/// Registers a callback listener for a specific topic.
///
/// The listener function will be called immediately whenever a message
/// is published to the topic. Returns a cancellation function to stop
/// listening.
///
/// ## Example
///
/// ```gleam
/// let whisper = whisper.new(10)
/// let cancel = whisper.on(whisper, "notifications", fn(msg) {
///   io.println("Received: " <> msg)
/// })
///
/// whisper.publish(whisper, "notifications", "Hello!")
/// // Prints: "Received: Hello!"
///
/// cancel()  // Stop listening
/// ```
pub fn on(
  whisper: Whisper(a),
  topic: String,
  listener: fn(a) -> Nil,
) -> fn() -> Nil {
  platform_on(whisper, topic, listener)
}

@external(erlang, "whisper_ffi", "on")
@external(javascript, "./whisper_ffi.mjs", "on")
fn platform_on(
  whisper: Whisper(a),
  topic: String,
  listener: fn(a) -> Nil,
) -> fn() -> Nil

/// Creates a subscription to a topic with a buffered message queue.
///
/// Messages published to the topic are stored in a buffer until retrieved
/// with `receive()`. The subscription's `receive()` function is non-blocking
/// and returns `Error(Nil)` if no messages are available.
///
/// ## Example
///
/// ```gleam
/// let whisper = whisper.new(10)
/// let sub = whisper.subscribe(whisper, "events")
///
/// whisper.publish(whisper, "events", 42)
///
/// case sub.receive() {
///   Ok(msg) -> io.debug(msg)  // 42
///   Error(Nil) -> io.println("No messages")
/// }
///
/// sub.cancel()  // Clean up
/// ```
pub fn subscribe(whisper: Whisper(a), topic: String) -> Subscription(a) {
  let #(receive_fn, cancel_fn) = platform_subscribe(whisper, topic)
  Subscription(receive: receive_fn, cancel: cancel_fn)
}

@external(erlang, "whisper_ffi", "subscribe")
@external(javascript, "./whisper_ffi.mjs", "subscribe")
fn platform_subscribe(
  whisper: Whisper(a),
  topic: String,
) -> #(fn() -> Result(a, Nil), fn() -> Nil)

/// Publishes a message to all listeners and subscribers of a topic.
///
/// The message is immediately delivered to all callback listeners and
/// added to the buffer of all subscriptions for the topic.
///
/// ## Example
///
/// ```gleam
/// let whisper = whisper.new(10)
///
/// let sub1 = whisper.subscribe(whisper, "chat")
/// let sub2 = whisper.subscribe(whisper, "chat")
///
/// whisper.publish(whisper, "chat", "Hello everyone!")
///
/// // Both subscribers will receive the message
/// let assert Ok(msg1) = sub1.receive()
/// let assert Ok(msg2) = sub2.receive()
/// ```
pub fn publish(whisper: Whisper(a), topic: String, message: a) -> Nil {
  platform_publish(whisper, topic, message)
}

@external(erlang, "whisper_ffi", "publish")
@external(javascript, "./whisper_ffi.mjs", "publish")
fn platform_publish(whisper: Whisper(a), topic: String, msg: a) -> Nil
