// slices/workspaceSlice.js
import { createSlice } from '@reduxjs/toolkit';

export const workspaceSlice = createSlice({
  name: 'workspace',
  initialState: {
    currentWorkspace: '',
  },
  reducers: {
    setWorkspace: (state, action) => {
      state.currentWorkspace = action.payload;
    },
  },
});

export const { setWorkspace } = workspaceSlice.actions;
export const getWorkspace = (state) => state.workspace.currentWorkspace;

export default workspaceSlice.reducer;
