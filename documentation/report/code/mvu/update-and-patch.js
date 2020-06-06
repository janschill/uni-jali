function updateAndPatch(message) {
  oldModel = model;
  newModel = update(message, oldModel);
  eval(patchToJs(diff(view(oldModel), view(newModel))));
  model = newModel;
}
