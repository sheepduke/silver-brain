const axios = require('axios')

export async function getAllConcepts() {
  let response = await axios.get(url(''))
  return response.data
}

/**
 * Create a concept.
 *
 * Return: UUID of newly created concept.
 */
export async function newConcept(name, content, contentFormat) {
  let data = {
    name: name,
    content: content,
    contentFormat: contentFormat
  }
  let response = await axios.post(url(''), data)
  if (response.status !== 201) {
    throw Error('Failed to add new concept')
  }
  return response.headers.location.substr(10)
}

export async function getConceptByUuid(uuid) {
  let response = await axios.get(url(`/${uuid}`))
  return response.data
}

export async function searchConcept(search) {
  let response = await axios.get(url(`?search=${search}`))
  return response.data
}

export async function updateConcept(concept) {
  await axios.put(url(`/${concept.uuid}`, concept))
}

export async function deleteConcept(uuid) {
  await axios.delete(url(`/${uuid}`))
}

/**
 * Return Array<Object{uuid, name}>
 */
export async function conceptParents(uuid) {
  let response = await axios.get(url(`/${uuid}/parents`))
  return response.data
}

export async function conceptChildren(uuid) {
  let response = await axios.get(url(`/${uuid}/children`))
  return response.data
}

export async function conceptFriends(uuid) {
  let response = await axios.get(url(`/${uuid}/friends`))
  return response.data
}

export async function removeRelation(relation, uuid, targetUuid) {
  await axios.delete(url(`/${uuid}/${relation}/${targetUuid}`))
}

export async function addRelation(relation, uuid, targetUuid) {
  await axios.put(url(`/${uuid}/${relation}/${targetUuid}`))
}

function url(part) {
  return `/api/concepts${part}`
}
