import gleam/deque.{type Deque}

pub opaque type BoundedBuffer(a) {
  BoundedBuffer(deque: Deque(a), capacity: Int, size: Int)
}

pub fn new_buffer(capacity: Int) -> BoundedBuffer(a) {
  BoundedBuffer(deque: deque.new(), capacity: capacity, size: 0)
}

pub fn push(buffer: BoundedBuffer(a), message: a) -> BoundedBuffer(a) {
  let new_deque = deque.push_back(buffer.deque, message)
  let new_size = buffer.size + 1

  case new_size > buffer.capacity {
    True -> {
      let assert Ok(#(_, remaining)) = deque.pop_front(new_deque)
      BoundedBuffer(..buffer, deque: remaining, size: buffer.capacity)
    }
    False -> BoundedBuffer(..buffer, deque: new_deque, size: new_size)
  }
}

pub fn pop(buffer: BoundedBuffer(a)) -> #(Result(a, Nil), BoundedBuffer(a)) {
  case deque.pop_front(buffer.deque) {
    Ok(#(message, remaining)) -> #(
      Ok(message),
      BoundedBuffer(..buffer, deque: remaining, size: buffer.size - 1),
    )
    Error(Nil) -> #(Error(Nil), buffer)
  }
}
