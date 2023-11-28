// store/store.js
import { configureStore } from '@reduxjs/toolkit'
import workspaceReducer from '../slices/workspaceSlice';

const store = configureStore({
    reducer: {
        workspace: workspaceReducer,
    },
});

export default store;
