import React, { useState, useEffect } from 'react';
import { styled, useTheme } from '@mui/material/styles';
import Box from '@mui/material/Box';
import Drawer from '@mui/material/Drawer';
import Toolbar from '@mui/material/Toolbar';
import List from '@mui/material/List';
import Divider from '@mui/material/Divider';
import ListItem from '@mui/material/ListItem';
import ListItemButton from '@mui/material/ListItemButton';
import ListItemText from '@mui/material/ListItemText';
import Button from '@mui/material/Button';
import api from '../../api/api';

const drawerWidth = 240;

const DrawerHeader = styled('div')(({ theme }) => ({
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'flex-end',
    padding: theme.spacing(0, 1),
    // necessary for content to be below app bar
    ...theme.mixins.toolbar,
  }));

function Navigation() {
  const [workspaces, setWorkSpaces] = useState([]);
  const [selectedIndex, setSelectedIndex] = React.useState(0);

  const handleListItemClick = (event, index) => {
    setSelectedIndex(index);
  };

  useEffect(() => {
    api.get('/api/v2/stores')
      .then((response) => {
        setWorkSpaces(response); 
      })
      .catch((error) => {
        console.error('Error fetching workspaces:', error);
        setWorkSpaces(['Workspace-0', 'Workspace-1', 'Workspace-2']); 
      });
  }, []);

  return (
    // <div>
    //   <ul>
    //     {workspaces.map((item) => (
    //       <li key={item.id}>{item.title}</li>
    //     ))}
    //   </ul>
    // </div>
    <Drawer
        variant="permanent"
        sx={{
        width: drawerWidth,
        flexShrink: 0,
        [`& .MuiDrawer-paper`]: { width: drawerWidth, boxSizing: 'border-box' },
        }}
        open={true}
    >
        <Toolbar />
        <Box sx={{ overflow: 'auto' }}>
        <DrawerHeader>
            {/* <IconButton onClick={handleDrawerClose}>
            {theme.direction === 'rtl' ? <ChevronRightIcon /> : <ChevronLeftIcon />}
            </IconButton> */}
        </DrawerHeader>
        <Divider textAlign="left">Workspaces</Divider>
        <Button variant="outlined">Create workspace</Button>
        <List>
            {workspaces.map((text, index) => (
            <ListItem key={text} disablePadding>
                <ListItemButton
                selected={selectedIndex === index}
                onClick={(event) => handleListItemClick(event, index)}
                >
                {/* <ListItemIcon>
                    {index % 2 === 0 ? <InboxIcon /> : <MailIcon />}
                </ListItemIcon> */}
                <ListItemText primary={text} />
                </ListItemButton>
            </ListItem>
            ))}
        </List>
        <Divider />
        </Box>
    </Drawer>
  );
}

export default Navigation;