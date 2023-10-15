import * as React from 'react';
import { styled, useTheme } from '@mui/material/styles';
import Box from '@mui/material/Box';
import Drawer from '@mui/material/Drawer';
import AppBar from '@mui/material/AppBar';
import CssBaseline from '@mui/material/CssBaseline';
import Toolbar from '@mui/material/Toolbar';
import List from '@mui/material/List';
import Typography from '@mui/material/Typography';
import Divider from '@mui/material/Divider';
import ListItem from '@mui/material/ListItem';
import ListItemButton from '@mui/material/ListItemButton';
import ListItemIcon from '@mui/material/ListItemIcon';
import ListItemText from '@mui/material/ListItemText';
import InboxIcon from '@mui/icons-material/MoveToInbox';
import MailIcon from '@mui/icons-material/Mail';
import IconButton from '@mui/material/IconButton';
import ChevronLeftIcon from '@mui/icons-material/ChevronLeft';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import Button from '@mui/material/Button';
import InputBase from '@mui/material/InputBase';
import TuneIcon from '@mui/icons-material/Tune';
import Stack from '@mui/material/Stack';
import './diagram.css';
import './App.css';

import Diagram, { createSchema, useSchema } from 'beautiful-react-diagrams';

import NewEntryModal from './components/NewEntryModal';
import EntryCard from './components/EntryCard';

const CustomNode = (props) => {
  const { inputs } = props;
  
  return (
    <div style={{ background: '#717EC3', borderRadius: '10px' }}>
      <div style={{ padding: '10px', color: 'white'  }}>
        Custom Node
      </div>
      <div style={{marginTop: '20px'}}>
        {inputs.map((port) => React.cloneElement(port, {
          style: { width: '50px', height: '25px', background: '#1B263B' }
        }))}
      </div>
    </div>
  );
};

const initialSchema = createSchema({
  nodes: [
    { 
      id: 'node-1', 
      content: 'Node 1', 
      coordinates: [150, 60], 
      outputs: [ { id: 'port-1', alignment: 'right' } ], 
    },
    { 
      id: 'node-custom', 
      coordinates: [250, 60], 
      render: CustomNode,
      inputs: [ { id: 'custom-port-1',  alignment: 'left' } ],
    },
  ]
});

const testSchema = createSchema({
  nodes: [
    { id: 'node-1', content: 'Node 1', coordinates: [250, 60], },
    { id: 'node-2', content: 'Node 2', coordinates: [100, 200], },
    { id: 'node-3', content: 'Node 3', coordinates: [250, 200], },
    { id: 'node-4', content: 'Node 4', coordinates: [400, 200], },
  ],
  links: [
    { input: 'node-1',  output: 'node-2' },
    { input: 'node-1',  output: 'node-3' },
    { input: 'node-1',  output: 'node-4' },
  ],
});

const UncontrolledDiagram = () => {
  // create diagrams schema
  const [schema, { onChange }] = useSchema(testSchema);

  return (
    <div style={{ height: '22.5rem', position: 'relative', }}>
      <Diagram schema={schema} onChange={onChange} />
    </div>
  );
};

const drawerWidth = 240;

const DrawerHeader = styled('div')(({ theme }) => ({
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'flex-end',
  padding: theme.spacing(0, 1),
  // necessary for content to be below app bar
  ...theme.mixins.toolbar,
}));

const ViewPanel = styled('div')(({ theme }) => ({
  backgroundColor: '#f5f6f8',
  padding: theme.spacing(0, 1),
  flexGrow: 1,
  // necessary for content to be below app bar
  ...theme.mixins.toolbar,
}));

const creationModalStyle = {
  position: 'absolute',
  top: '50%',
  left: '50%',
  transform: 'translate(-50%, -50%)',
  width: 400,
  backgroundColor: 'white',
  border: '2px solid #000',
  boxShadow: 24,
  p: 4,
};

export default function App() {
  const theme = useTheme();
  const [open, setOpen] = React.useState(true);
  const [isNewEntryModalOpen, setNewEntryModalOpen] = React.useState(false);

  const handleDrawerOpen = () => {
    setOpen(true);
  };

  const handleDrawerClose = () => {
    setOpen(false);
  };

  const handleCreationModalOpen = () => {
    setNewEntryModalOpen(true);
  };

  const handleClose = () => {
    setNewEntryModalOpen(false);
  };

  return (
    <Box sx={{ display: 'flex', height: '100%' }}>
      <CssBaseline />
      <AppBar position="fixed" sx={{ zIndex: (theme) => theme.zIndex.drawer + 1 }}>
        <Toolbar>
          <Typography variant="h6" noWrap component="div">
            App Bar
          </Typography>
        </Toolbar>
      </AppBar>
      <Drawer
        variant="permanent"
        sx={{
          width: drawerWidth,
          flexShrink: 0,
          [`& .MuiDrawer-paper`]: { width: drawerWidth, boxSizing: 'border-box' },
        }}
        open={open}
      >
        <Toolbar />
        <Box sx={{ overflow: 'auto' }}>
          <DrawerHeader>
            <IconButton onClick={handleDrawerClose}>
              {theme.direction === 'rtl' ? <ChevronRightIcon /> : <ChevronLeftIcon />}
            </IconButton>
          </DrawerHeader>
          <List>
            {['A', 'B', 'C', 'D'].map((text, index) => (
              <ListItem key={text} disablePadding>
                <ListItemButton>
                  <ListItemIcon>
                    {index % 2 === 0 ? <InboxIcon /> : <MailIcon />}
                  </ListItemIcon>
                  <ListItemText primary={text} />
                </ListItemButton>
              </ListItem>
            ))}
          </List>
          <Divider />
          <List>
            {['E', 'F', 'G'].map((text, index) => (
              <ListItem key={text} disablePadding>
                <ListItemButton>
                  <ListItemIcon>
                    {index % 2 === 0 ? <InboxIcon /> : <MailIcon />}
                  </ListItemIcon>
                  <ListItemText primary={text} />
                </ListItemButton>
              </ListItem>
            ))}
          </List>
        </Box>
      </Drawer>
      <Box component="main" sx={{ flexGrow: 1, p: 0, height: '100%', display: 'flex', flexDirection: 'column' }}>
        <Toolbar />
        <ViewPanel >
          <Toolbar sx={{ backgroundColor: 'white', marginBottom: '10px', }}>
            <Button onClick={handleCreationModalOpen} variant="contained" sx={{ textTransform: 'none', marginRight: '14px', }}>New Entry</Button>
            <Stack
              direction="row"
              sx={{ border: '1px solid rgba(0, 0, 0, 0.12)', borderRadius: '12px', boxShadow: '0px 2px 1px -1px rgba(0,0,0,0.2), 0px 1px 1px 0px rgba(0,0,0,0.14), 0px 1px 3px 0px rgba(0,0,0,0.12)' }}
            >
              <InputBase
                sx={{ ml: 1, flex: 1 }}
                placeholder="Search"
                inputProps={{ 'aria-label': 'search' }}
              />
              <IconButton type="button" sx={{ p: '10px' }} aria-label="search options">
                <TuneIcon />
              </IconButton>
            </Stack>
          </Toolbar>
          {/* <EntryCard /> */}
          <UncontrolledDiagram />
        </ViewPanel>
      </Box>
      <NewEntryModal
        isOpen={isNewEntryModalOpen}
        onClose={() => setNewEntryModalOpen(false)}
      />
      {/* <Modal
        open={creationModalOpen}
        onClose={handleClose}
        aria-labelledby="modal-modal-title"
        aria-describedby="modal-modal-description"
      >
        <Box style={creationModalStyle}>
          <div >Create new entry modal.</div>
        </Box>
      </Modal> */}
    </Box>
  );
}