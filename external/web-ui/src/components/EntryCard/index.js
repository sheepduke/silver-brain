import React, { useState } from 'react';
import { IconButton, Box, Stack, Chip, Badge, Drawer } from '@mui/material';
import AttachmentIcon from '@mui/icons-material/Attachment';
import ShortTextOutlinedIcon from '@mui/icons-material/ShortTextOutlined';
import BookmarksOutlinedIcon from '@mui/icons-material/BookmarksOutlined';
import LinkIcon from '@mui/icons-material/Link';
import Toolbar from '@mui/material/Toolbar';
import Link from '@mui/material/Link';
import './index.css';

const EntryCard = ({ isOpen, onClose, onSave }) => {
  const [attachmentsCount, setAttachmentsCount] = useState(2);
  const [isDrawerOpen, setDrawerOpen] = useState(false);

  return (
    <Box className="entry-card-wrapper">
      <div className="content-wrapper">
        The content of the entry 1.
        Any text content can be displayed in this area. <br />
        TODO: The markdown text should be displayed in correct style.
      </div>
      <div className="info-wrapper">
        <div className="card-header">
          <div className="title">Entry 1</div>
          <div className="attachments-indicator-wrapper">
            <IconButton onClick={() => setDrawerOpen(true)}>
              <Badge badgeContent={attachmentsCount} color="primary">
                <AttachmentIcon color={attachmentsCount > 0 ? 'primary' : 'action'} />
              </Badge>
            </IconButton>
          </div>
        </div>
        <div className="single-row-wrapper summary">
          <div className="row-title-wrapper">
            <LinkIcon className="normal-icon" />
          </div>
          <div className="row-content">
            <Stack direction="row" spacing={1}>
              <Link href="#" underline="always">
                {'Link1'}
              </Link>
              <Link href="#" underline="always">
                {'Link2'}
              </Link>
              <Link href="#" underline="always">
                {'Link3'}
              </Link>

            </Stack>
          </div>
        </div>
        <div className="single-row-wrapper tags-container">
          <div className="row-title-wrapper">
            <BookmarksOutlinedIcon className="normal-icon" />
            {/* Tags: */}
          </div>
          <div className="row-content">
            <Stack direction="row" spacing={1}>
              <Chip label="test" size="small" variant="outlined" />
              <Chip label="demo" size="small" variant="outlined" />
              <Chip label="tags" size="small" variant="outlined" />
            </Stack>
          </div>
        </div>
      </div>
      <Drawer
        anchor="right"
        open={isDrawerOpen}
        onClose={() => setDrawerOpen(false)}
      >
        <Toolbar style={{ width: '400px' }}></Toolbar>
        Edit attachments drawer
      </Drawer>
    </Box>
  );
};

export default EntryCard;
