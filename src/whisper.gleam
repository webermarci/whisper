import gleam/result

pub type ReceiveError {
  TopicIsNotRegistered
  SubscriptionCancelled
}

pub type ReceiveResult(a) {
  Message(a)
  Empty
  Closed(ReceiveError)
}

pub type Subscription(a) {
  Subscription(receive: fn() -> ReceiveResult(a), cancel: fn() -> Nil)
}

pub type TopicError {
  InvalidTopic
  InvalidCapacity
  AlreadyRegistered
}

pub type Whisper(a) {
  Whisper(topic: String, capacity: Int)
}

/// Creates a new topic with the specified capacity for message buffering.
///
/// The topic name must be non-empty and the capacity must be greater than zero.
/// Returns an error if the topic name is invalid, capacity is invalid, or the
/// topic is already registered.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(whisper) = new_topic("notifications", capacity: 100)
/// ```
pub fn new_topic(topic: String, capacity: Int) -> Result(Whisper(a), TopicError) {
  use topic <- result.try(validate_topic(topic))
  use capacity <- result.try(validate_capacity(capacity))
  platform_register(topic, capacity)
}

@external(erlang, "whisper_ffi", "register")
@external(javascript, "./whisper_ffi.mjs", "register")
fn platform_register(
  topic: String,
  capacity: Int,
) -> Result(Whisper(a), TopicError)

/// Registers a listener function that will be called whenever a message is
/// published to the topic.
///
/// Returns a function that can be called to unregister the listener. Multiple
/// listeners can be registered on the same topic, and each will receive all
/// published messages.
///
/// ## Examples
///
/// ```gleam
/// let close = on(whisper, fn(message) {
///   io.println("Received: " <> message)
/// })
/// // Later, to stop listening:
/// close()
/// ```
pub fn on(whisper: Whisper(a), listener: fn(a) -> Nil) -> fn() -> Nil {
  platform_on(whisper.topic, listener)
}

@external(erlang, "whisper_ffi", "on")
@external(javascript, "./whisper_ffi.mjs", "on")
fn platform_on(topic: String, listener: fn(a) -> Nil) -> fn() -> Nil

/// Creates a subscription that allows pulling messages from the topic on demand.
///
/// Unlike `on`, which pushes messages to a callback, `subscribe` returns a
/// `Subscription` that can be polled using its `receive` function. The subscription
/// maintains its own message queue up to the topic's capacity.
///
/// Call the subscription's `cancel` function to stop receiving messages.
///
/// ## Examples
///
/// ```gleam
/// let sub = subscribe(whisper)
/// case sub.receive() {
///   Message(msg) -> io.println("Got: " <> msg)
///   Empty -> io.println("No messages")
///   Closed(err) -> io.println("Subscription closed")
/// }
/// // Later, to cancel:
/// sub.cancel()
/// ```
pub fn subscribe(whisper: Whisper(a)) -> Subscription(a) {
  let #(receive_fn, cancel_fn) = platform_subscribe(whisper.topic)
  Subscription(receive: receive_fn, cancel: cancel_fn)
}

@external(erlang, "whisper_ffi", "subscribe")
@external(javascript, "./whisper_ffi.mjs", "subscribe")
fn platform_subscribe(topic: String) -> #(fn() -> ReceiveResult(a), fn() -> Nil)

/// Publishes a message to all listeners and subscribers of the topic.
///
/// The message will be queued for each subscription up to the topic's capacity.
/// If a subscription's queue is full, older messages may be dropped depending on
/// the platform implementation.
///
/// ## Examples
///
/// ```gleam
/// publish(whisper, "Hello, world!")
/// ```
pub fn publish(whisper: Whisper(a), message: a) -> Nil {
  platform_publish(whisper.topic, message)
}

@external(erlang, "whisper_ffi", "publish")
@external(javascript, "./whisper_ffi.mjs", "publish")
fn platform_publish(topic: String, message: a) -> Nil

/// Closes the topic and notifies all active subscriptions.
///
/// After closing, subscribers will receive `Closed(TopicIsNotRegistered)` when
/// attempting to receive messages, and new subscriptions or listeners cannot be
/// created for this topic.
///
/// ## Examples
///
/// ```gleam
/// close(whisper)
/// ```
pub fn close(whisper: Whisper(a)) -> Nil {
  platform_close(whisper.topic)
}

@external(erlang, "whisper_ffi", "close")
@external(javascript, "./whisper_ffi.mjs", "close")
fn platform_close(topic: String) -> Nil

fn validate_topic(topic: String) -> Result(String, TopicError) {
  case topic {
    "" -> Error(InvalidTopic)
    _ -> Ok(topic)
  }
}

fn validate_capacity(capacity: Int) -> Result(Int, TopicError) {
  case capacity {
    c if c > 0 -> Ok(c)
    _ -> Error(InvalidCapacity)
  }
}
