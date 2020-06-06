let model = 0;
class Message { }
class Increment extends Message { }
class Decrement extends Message { }
const Inc = new Increment();
const Dec = new Decrement();

class Node { }
class Tag extends Node {
  constructor(elementName, attributes, children) {
    super();
    this.elementName = elementName;
    this.attributes = attributes;
    this.children = children;
  }
}
class Text extends Node {
  constructor(text) {
    super();
    this.text = text;
  }
}

class Differ { }
class Null extends Differ { }
class Change extends Differ {
  constructor(node) {
    super();
    this.node = node;
  }
}
class Path extends Differ {
  constructor(index, differ) {
    super();
    this.index = index;
    this.differ = differ;
  }
}

class Action { }
class Click extends Action { }
