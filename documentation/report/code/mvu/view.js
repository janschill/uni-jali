function view(model) {
  return new Tag('div', [], [
    new Tag('button', [(new Click(), 'updateAndPatch(new Increment())')], [
      new Text('Increment')
    ]),
    new Tag('p', [], [new Text(model)]),
    new Tag('button', [(new Click(), 'updateAndPatch(new Decrement())')], [
      new Text('Decrement')
    ]),
  ]);
}
