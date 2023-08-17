const removeParent = (element) => {
  element.parentElement.remove()
}

const removeClosest = (element, selector) => {
  htmx.closest(element, selector).remove()
}

const addTxtInput = () => {
  const template = document.querySelector('#txt-source-template')
  useSourceTemplate(template)
}

const addTomlInput = () => {
  const template = document.querySelector('#toml-source-template')
  useSourceTemplate(template)
}

const useSourceTemplate = (templateElement) => {
  const newSource = templateElement.cloneNode(true)
  newSource.classList.remove('hidden')
  newSource.removeAttribute('id')

  const sourcesFormGroup = document.querySelector('#sources')
  sourcesFormGroup.appendChild(newSource)
}
