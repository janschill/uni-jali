function changeInnerHTML(seq, value) {
  const element = ((sq) => {
    function loop(str, list) {
      if (list.length === 0) {
        return str;
      } else {
        return loop(`${str}.children[${list[0]}]`, list.slice(1));
      }
    }
    return loop('document.body', seq);
  })(seq);
  return `${element}.innerHTML = ${value};`;
}

function patchToJs(changes) {
  function loop(seq, chs) {
    if (chs instanceof Null) {
      return 'patchToJs: No changes detected';
    } else if (chs instanceof Path) {
      return loop(seq.concat(chs.index), chs.differ);
    } else {
      const node = chs.node;
      if (node instanceof Text) {
        return changeInnerHTML(seq, node.text);
      } else {
        return 'patchToJs: Not implemented: changeElement(node, seq)';
      }
    }
  }
  return loop([0], changes);
}
