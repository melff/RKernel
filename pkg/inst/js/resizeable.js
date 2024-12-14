function render({ model, el }) {
  let _innerdiv = document.createElement('div');
  if (model.get('debug')) {
    el.classList.add('debug-resizer');
  }
  _innerdiv.classList.add('resized');
  _innerdiv.innerHTML = model.get('value');
  el.appendChild(_innerdiv);
  let rob = new ResizeObserver((entries) => {
    const width = parseInt(el.style.width);
    const height = parseInt(el.style.height);
    model.set('width', width);
    model.set('height', height);
    model.save_changes();
  });
  rob.observe(el);
  model.on('change:value', () => {
    _innerdiv.innerHTML = model.get('value');
  });
  model.on('change:debug', () => {
    if (model.get('debug')) {
      el.classList.add('debug-resizer');
    } else {
      el.classList.remove('debug-resizer');
    }
  });
  model.on('change:width', () => {
    const width = model.get('width');
    const height = model.get('height');
    if (width > 0) {
      el.style.width = `${width}px`;
    }
    if (height > 0) {
      el.style.height = `${height}px`;
    }
  });
  model.on('change:height', () => {
    const width = model.get('width');
    const height = model.get('height');
    if (width > 0) {
      el.style.width = `${width}px`;
    }
    if (height > 0) {
      el.style.height = `${height}px`;
    }
  });
  const direction = model.get('direction');
  if (direction == 'both') {
    el.classList.add('resizer');
  }
  if (direction == 'vertical') {
    el.classList.add('vresizer');
  }
  if (direction == 'horizontal') {
    el.classList.add('hresizer');
  }
  width = parseInt(el.style.width);
  height = parseInt(el.style.height);
  model.set('width', width);
  model.set('height', height);
  model.save_changes();
}

export default { render };
