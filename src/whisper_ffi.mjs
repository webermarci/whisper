import { Ok, Error } from "./gleam.mjs";

export function new_whisper(capacity) {
  return {
    listeners: new Map(),
    subscribers: new Map(),
    capacity: capacity,
    nextId: 0,
  };
}

export function on(whisper, topic, listener) {
  const id = whisper.nextId++;

  if (!whisper.listeners.has(topic)) {
    whisper.listeners.set(topic, new Map());
  }
  whisper.listeners.get(topic).set(id, listener);

  return () => {
    const topicListeners = whisper.listeners.get(topic);
    if (topicListeners) {
      topicListeners.delete(id);
    }
  };
}

export function subscribe(whisper, topic) {
  const id = whisper.nextId++;
  const buffer = [];

  if (!whisper.subscribers.has(topic)) {
    whisper.subscribers.set(topic, new Map());
  }
  whisper.subscribers.get(topic).set(id, buffer);

  const receive = () => {
    if (buffer.length > 0) {
      return new Ok(buffer.shift());
    }
    return new Error(null);
  };

  const cancel = () => {
    const topicSubs = whisper.subscribers.get(topic);
    if (topicSubs) {
      topicSubs.delete(id);
    }
    buffer.length = 0;
  };

  return [receive, cancel];
}

export function publish(whisper, topic, message) {
  const topicListeners = whisper.listeners.get(topic);
  if (topicListeners) {
    topicListeners.forEach((listener) => listener(message));
  }

  const topicSubs = whisper.subscribers.get(topic);
  if (topicSubs) {
    topicSubs.forEach((buffer) => {
      buffer.push(message);
      if (buffer.length > whisper.capacity) buffer.shift();
    });
  }
}
