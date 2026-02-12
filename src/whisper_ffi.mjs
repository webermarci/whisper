import { Ok, Error } from "./gleam.mjs";
import {
  Whisper,
  AlreadyRegistered,
  TopicIsNotRegistered,
  SubscriptionCancelled,
  Message,
  Empty,
  Closed,
} from "./whisper.mjs";
import { new_buffer, push, pop } from "./whisper/buffer.mjs";

const registry = new Map();

export function register(topic, capacity) {
  if (registry.has(topic)) {
    return new Error(new AlreadyRegistered());
  } else {
    registry.set(topic, {
      topic: topic,
      capacity: capacity,
      listeners: [],
      subscribers: [],
      nextId: 0,
    });
    return new Ok(new Whisper(topic, capacity));
  }
}

export function on(topic, listener) {
  const whisper = registry.get(topic);
  if (!whisper) {
    return () => {};
  }

  whisper.nextId++;
  const listenerId = whisper.nextId;
  whisper.listeners.push({ id: listenerId, fn: listener });

  return () => {
    const index = whisper.listeners.findIndex((l) => l.id === listenerId);
    if (index !== -1) {
      whisper.listeners.splice(index, 1);
    }
  };
}

export function subscribe(topic) {
  const whisper = registry.get(topic);
  if (!whisper) {
    return [() => new Closed(new TopicIsNotRegistered()), () => {}];
  }

  whisper.nextId++;
  const subscriberId = whisper.nextId;
  let buffer = new_buffer(whisper.capacity);
  whisper.subscribers.push({ id: subscriberId, buffer: buffer });

  const receive = () => {
    const whisper = registry.get(topic);
    if (!whisper) {
      return new Closed(new TopicIsNotRegistered());
    }

    const subscriber = whisper.subscribers.find((s) => s.id === subscriberId);
    if (!subscriber) {
      return new Closed(new SubscriptionCancelled());
    }

    const [result, newBuffer] = pop(subscriber.buffer);
    subscriber.buffer = newBuffer;

    if (result.isOk()) {
      return new Message(result[0]);
    } else {
      return new Empty();
    }
  };

  const cancel = () => {
    const whisper = registry.get(topic);
    if (!whisper) return;

    const index = whisper.subscribers.findIndex((s) => s.id === subscriberId);
    if (index !== -1) {
      whisper.subscribers.splice(index, 1);
    }
  };

  return [receive, cancel];
}

export function publish(topic, message) {
  const whisper = registry.get(topic);
  if (!whisper) {
    return;
  }

  whisper.listeners.forEach((listener) => {
    listener.fn(message);
  });

  whisper.subscribers.forEach((subscriber) => {
    subscriber.buffer = push(subscriber.buffer, message);
  });
}

export function close(topic) {
  registry.delete(topic);
}
