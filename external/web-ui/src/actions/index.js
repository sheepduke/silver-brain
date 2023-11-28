// actions/index.js
export const SET_WORKSPACE = 'SET_WORKSPACE';

export const setWorkspace = (workspace) => ({
  type: SET_WORKSPACE,
  payload: workspace,
});
