function update(message, model_) {
  if (message instanceof Increment) {
    return ++model_;
  } else {
    return --model_;
  }
}
