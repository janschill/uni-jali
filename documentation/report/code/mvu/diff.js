function diff(view1, view2) {
  if (view1 instanceof Tag && view2 instanceof Tag) {
    if (view1.elementName === view2.elementName) {
      function fold(nodes1, nodes2, index) {
        if (Array.isArray(nodes1)
          && Array.isArray(nodes2)
          && nodes1.length > 0 && nodes2.length > 0) {
          const tail1 = nodes1.slice(1);
          const tail2 = nodes2.slice(1);
          const change = diff(nodes1[0], nodes2[0]);
          if (change instanceof Null) {
            return fold(tail1, tail2, ++index);
          } else {
            return new Path(index, change)
          }
        } else {
          return new Null();
        }
      }
      return fold(view1.children, view2.children, 0);
    } else {
      return new Change(new Tag(view2.elementName, view2.attributes, view2.children));
    }
  } else {
    return view1.text === view2.text ? new Null() : new Change(new Text(view2.text));
  }
}
