const axios = require('axios')

export async function getAllConcepts () {
  let response = await axios.get('/concepts')
  return response.data
}

/**
 * Create a concept.
 *
 * Return: UUID of newly created concept.
 */
export async function newConcept (name, content, contentFormat) {
  let data = {
    name: name,
    content: content,
    contentFormat: contentFormat
  }
  let response = await axios.post('/concepts', data)
  if (response.status !== 201) {
    throw Error('Failed to add new concept')
  }
  return response.headers.location
}

export async function getConceptByUuid (uuid) {
  let response = await axios.get(`/concepts/${uuid}`)
  return response.data
}

export async function searchConcept (search) {
  let response = await axios.get(`/concepts?search=${search}`)
  return response.data
}

/**
 * Return Array<Object{uuid, name}>
 */
export async function conceptParents (uuid) {
  let response = await axios.get(`/concepts/${uuid}/parents`)
  return response.data
}

export async function conceptChildren (uuid) {
  let response = await axios.get(`/concepts/${uuid}/children`)
  return response.data
}

export async function conceptFriends (uuid) {
  let response = await axios.get(`/concepts/${uuid}/friends`)
  return response.data
}

export async function removeRelation (relation, uuid, targetUuid) {
  await axios.delete(`/concepts/${uuid}/${relation}/${targetUuid}`)
}

export async function addRelation (relation, uuid, targetUuid) {
  await axios.put(`/concepts/${uuid}/${relation}/${targetUuid}`)
}
