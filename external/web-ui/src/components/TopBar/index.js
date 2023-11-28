import React, { useState, useEffect } from 'react';
import { styled, alpha } from '@mui/material/styles';
import Toolbar from '@mui/material/Toolbar';
import Button from '@mui/material/Button';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import MoreHorizIcon from '@mui/icons-material/MoreHoriz';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import AppBar from '@mui/material/AppBar';
import api from '../../api/api';

import { useDispatch } from 'react-redux';
import { setWorkspace } from '../../slices/workspaceSlice';

import './index.css';

const StyledMenu = styled((props) => (
  <Menu
    elevation={0}
    anchorOrigin={{
      vertical: 'bottom',
      horizontal: 'right',
    }}
    transformOrigin={{
      vertical: 'top',
      horizontal: 'right',
    }}
    {...props}
  />
))(({ theme }) => ({
  '& .MuiPaper-root': {
    borderRadius: 6,
    marginTop: theme.spacing(1),
    minWidth: 180,
    color:
      theme.palette.mode === 'light' ? 'rgb(55, 65, 81)' : theme.palette.grey[300],
    boxShadow:
      'rgb(255, 255, 255) 0px 0px 0px 0px, rgba(0, 0, 0, 0.05) 0px 0px 0px 1px, rgba(0, 0, 0, 0.1) 0px 10px 15px -3px, rgba(0, 0, 0, 0.05) 0px 4px 6px -2px',
    '& .MuiMenu-list': {
      padding: '4px 0',
    },
    '& .MuiMenuItem-root': {
      '& .MuiSvgIcon-root': {
        fontSize: 18,
        color: theme.palette.text.secondary,
        marginRight: theme.spacing(1.5),
      },
      '&:active': {
        backgroundColor: alpha(
          theme.palette.primary.main,
          theme.palette.action.selectedOpacity,
        ),
      },
    },
  },
}));

function TopBar() {
  const dispatch = useDispatch();

  const [currentWorkspace, setCurrentWorkspace] = useState('');
  const [workspaces, setWorkSpaces] = useState([]);
  const [anchorEl, setAnchorEl] = React.useState(null);
  const open = Boolean(anchorEl);
  const handleClick = (event) => {
    setAnchorEl(event.currentTarget);
  };
  const handleClose = () => {
    setAnchorEl(null);
  };

  const onClickWorkspace = (index) => {
    const curWorkspace = workspaces[index];
    dispatch(setWorkspace(curWorkspace));
    setCurrentWorkspace(curWorkspace);
    api.get('/api/v2/stores');
    handleClose();
  };

  useEffect(() => {
    api.get('/api/v2/stores')
      .then((response) => {
        setWorkSpaces(response); 
      })
      .catch((error) => {
        console.error('Error fetching workspaces:', error);
        const mockWorkspaces = ['Workspace-0', 'Workspace-1', 'Workspace-2'];
        setWorkSpaces(mockWorkspaces);
        setCurrentWorkspace(mockWorkspaces[0]);
        dispatch(setWorkspace(mockWorkspaces[0]));
      });
  }, []);

  return (
    <AppBar position="fixed" sx={{ zIndex: (theme) => theme.zIndex.drawer + 1 }}>
        <Toolbar>
            <Button
                id="switch-workspace-button"
                aria-controls={open ? 'workspace-menu' : undefined}
                aria-haspopup="true"
                aria-expanded={open ? 'true' : undefined}
                variant="contained"
                disableElevation
                onClick={handleClick}
                endIcon={<KeyboardArrowDownIcon />}
            >
                {currentWorkspace}
            </Button>
            <StyledMenu
              id="workspace-menu"
              MenuListProps={{
                'aria-labelledby': 'switch-workspace-button',
              }}
              anchorEl={anchorEl}
              open={open}
              onClose={handleClose}
            >
              <MenuItem onClick={handleClose} disableRipple divider>
                <Button variant="outlined">Create workspace</Button>
              </MenuItem>
              {workspaces.map((text, index) => (
              <MenuItem className='workspace-item' onClick={onClickWorkspace.bind(this, index)} disableRipple>
                  {text}
                  <MoreHorizIcon />
              </MenuItem>
              ))}
            </StyledMenu>
        </Toolbar>
    </AppBar>
  );
}

export default TopBar;