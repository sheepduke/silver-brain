import axios from 'axios';
import store from '../store/store';
import { getWorkspace } from '../slices/workspaceSlice';

const api = axios.create({
  baseURL: 'https://silver-brain-api-url.com',
  timeout: 10000,
});

// Set request interceptor
api.interceptors.request.use((config) => {
  const token = localStorage.getItem('token');
  if (token) {
    config.headers['Authorization'] = `Bearer ${token}`;
  }
  const currentWorkspace = getWorkspace(store.getState());
  config.headers['X-Workspace'] = `sp ${currentWorkspace}`;
  return config;
}, (error) => {
  return Promise.reject(error);
});

// Set reponse interceptor
api.interceptors.response.use((response) => {
  // Resolve response code
  if (response.status === 200) {
    return response.data;
  } else {
    throw new Error(`HTTP Error: ${response.status}`);
  }
}, (error) => {
  // Resolve request error
  if (error.response) {
    // Request fail
    const { status, data } = error.response;
    throw new Error(`HTTP Error: ${status} - ${data.message}`);
  } else if (error.request) {
    // No response
    throw new Error('No response received from the server.');
  } else {
    // Request setup error
    throw new Error('Request setup error: ' + error.message);
  }
});

export default api;
