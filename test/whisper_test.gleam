import gleam/list
import gleeunit
import gleeunit/should
import whisper

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn callback_test() {
  let g = whisper.new(10)
  let cancel = whisper.on(g, "test", fn(msg) { should.equal(msg, "hello") })
  whisper.publish(g, "test", "hello")
  cancel()
}

pub fn subscribe_test() {
  let g = whisper.new(10)
  let sub = whisper.subscribe(g, "events")

  whisper.publish(g, "events", 42)

  let assert Ok(42) = wait_for_message(sub.receive, 10)

  sub.cancel()
}

pub fn multiple_subscribers_test() {
  let g = whisper.new(10)
  let sub1 = whisper.subscribe(g, "news")
  let sub2 = whisper.subscribe(g, "news")

  whisper.publish(g, "news", "breaking")

  let assert Ok("breaking") = wait_for_message(sub1.receive, 10)
  let assert Ok("breaking") = wait_for_message(sub2.receive, 10)

  sub1.cancel()
  sub2.cancel()
}

pub fn different_topics_test() {
  let g = whisper.new(10)
  let sub1 = whisper.subscribe(g, "topic1")
  let sub2 = whisper.subscribe(g, "topic2")

  whisper.publish(g, "topic1", 1)
  whisper.publish(g, "topic2", 2)

  let assert Ok(1) = wait_for_message(sub1.receive, 10)
  let assert Ok(2) = wait_for_message(sub2.receive, 10)

  sub1.cancel()
  sub2.cancel()
}

pub fn cancel_stops_receiving_test() {
  let g = whisper.new(10)
  let sub = whisper.subscribe(g, "events")

  whisper.publish(g, "events", 1)
  let assert Ok(1) = wait_for_message(sub.receive, 10)

  sub.cancel()

  whisper.publish(g, "events", 2)

  let assert Error(Nil) = wait_for_message(sub.receive, 10)
}

pub fn empty_buffer_test() {
  let g = whisper.new(10)
  let sub = whisper.subscribe(g, "events")

  let assert Error(Nil) = wait_for_message(sub.receive, 10)

  sub.cancel()
}

fn wait_for_message(
  receive_fn: fn() -> Result(a, Nil),
  max_attempts: Int,
) -> Result(a, Nil) {
  case receive_fn() {
    Ok(msg) -> Ok(msg)
    Error(Nil) if max_attempts > 0 -> {
      // Small busy-wait delay (cross-platform)
      let _ = list.range(0, 50)
      wait_for_message(receive_fn, max_attempts - 1)
    }
    Error(Nil) -> Error(Nil)
  }
}
