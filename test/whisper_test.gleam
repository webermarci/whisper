import gleam/int
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import whisper

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn callback_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let cancel = pubsub |> whisper.on(fn(msg) { should.equal(msg, "hello") })

  pubsub |> whisper.publish("hello")

  Ok(cancel())
}

pub fn callback_multiple_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let cancel1 = pubsub |> whisper.on(fn(msg) { should.equal(msg, "hello") })
  let cancel2 = pubsub |> whisper.on(fn(msg) { should.equal(msg, "hello") })

  pubsub |> whisper.publish("hello")

  Ok(list.each([cancel1, cancel2], fn(cancel) { cancel() }))
}

pub fn subscribe_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let sub = pubsub |> whisper.subscribe()

  pubsub |> whisper.publish("hello")
  let assert whisper.Message("hello") = wait_for_message(sub.receive, 10)

  Ok(sub.cancel())
}

pub fn subscribe_multiple_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let sub1 = pubsub |> whisper.subscribe()
  let sub2 = pubsub |> whisper.subscribe()

  pubsub |> whisper.publish("hello")

  let assert whisper.Message("hello") = wait_for_message(sub1.receive, 10)
  let assert whisper.Message("hello") = wait_for_message(sub2.receive, 10)

  Ok(list.each([sub1.cancel, sub2.cancel], fn(cancel) { cancel() }))
}

pub fn subscribe_cancel_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let sub1 = pubsub |> whisper.subscribe()
  let sub2 = pubsub |> whisper.subscribe()

  pubsub |> whisper.publish("hello")

  let assert whisper.Message("hello") = wait_for_message(sub1.receive, 10)
  let assert whisper.Message("hello") = wait_for_message(sub2.receive, 10)

  sub1.cancel()

  pubsub |> whisper.publish("world")

  let assert whisper.Message("world") = wait_for_message(sub2.receive, 10)
  let assert whisper.Closed(whisper.SubscriptionCancelled) =
    wait_for_message(sub1.receive, 10)

  Ok(sub2.cancel())
}

pub fn subscribe_capacity_test() {
  use pubsub <- result.try(whisper.new_topic("test", 2))
  let sub = pubsub |> whisper.subscribe()

  pubsub |> whisper.publish("msg1")
  pubsub |> whisper.publish("msg2")
  pubsub |> whisper.publish("msg3")

  let assert whisper.Message("msg2") = wait_for_message(sub.receive, 10)
  let assert whisper.Message("msg3") = wait_for_message(sub.receive, 10)
  let assert whisper.Empty = wait_for_message(sub.receive, 10)

  Ok(sub.cancel())
}

pub fn subscribe_empty_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let sub = pubsub |> whisper.subscribe()

  let assert whisper.Empty = wait_for_message(sub.receive, 10)

  Ok(sub.cancel())
}

pub fn subscribe_closed_test() {
  use pubsub <- result.try(whisper.new_topic("test", 10))
  let sub = pubsub |> whisper.subscribe()

  pubsub |> whisper.close()

  let assert whisper.Closed(whisper.TopicIsNotRegistered) =
    wait_for_message(sub.receive, 10)

  Ok(sub.cancel())
}

fn wait_for_message(
  receive_fn: fn() -> whisper.ReceiveResult(a),
  max_attempts: Int,
) -> whisper.ReceiveResult(a) {
  case receive_fn() {
    whisper.Message(message) -> whisper.Message(message)
    whisper.Empty -> {
      case max_attempts {
        0 -> whisper.Empty
        _ -> {
          // Small busy-wait delay (cross-platform)
          let _ = int.range(from: 0, to: 50, with: "", run: fn(_, _) { "" })
          wait_for_message(receive_fn, max_attempts - 1)
        }
      }
    }
    whisper.Closed(err) -> whisper.Closed(err)
  }
}

pub fn register_duplicate_test() {
  use pubsub <- result.try(whisper.new_topic("dup", 1))
  let assert Error(whisper.AlreadyRegistered) = whisper.new_topic("dup", 1)
  Ok(pubsub |> whisper.close())
}

pub fn subscribe_unregistered_test() {
  let sub = whisper.subscribe(whisper.Whisper("nope", 1))
  let assert whisper.Closed(whisper.TopicIsNotRegistered) =
    wait_for_message(sub.receive, 10)
  Ok(sub.cancel())
}

pub fn close_and_reregister_test() {
  use pubsub <- result.try(whisper.new_topic("tmp", 2))
  let sub = pubsub |> whisper.subscribe()
  pubsub |> whisper.close()
  let assert whisper.Closed(whisper.TopicIsNotRegistered) =
    wait_for_message(sub.receive, 10)
  use _ <- result.try(whisper.new_topic("tmp", 2))
  Ok(sub.cancel())
}

pub fn publish_noop_test() {
  // Publishing to an unregistered topic should be a safe no-op
  whisper.publish(whisper.Whisper("not_registered", 1), "hello")
  // create and close a real topic to produce a Nil payload for Ok
  use cleanup <- result.try(whisper.new_topic("cleanup", 1))
  Ok(cleanup |> whisper.close())
}
