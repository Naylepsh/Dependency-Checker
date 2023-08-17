const removeParent = (element) => {
  element.parentElement.remove()
}

const removeClosest = (element, selector) => {
  htmx.closest(element, selector).remove()
}
