import React, { useState } from 'react';
import { Modal, Button, IconButton, Box, TextField, Select, MenuItem } from '@mui/material';
import CloseIcon from '@mui/icons-material/Close';
import './index.css';

const NewEntryModal = ({ isOpen, onClose, onSave }) => {
  const [name, setName] = useState('New Entry');
  const [summary, setSummary] = useState('');
  const [contentType, setContentType] = useState('');
  const [content, setContent] = useState('');

  const handleSave = () => {
    // 构造新的entry对象
    const newEntry = {
      name,
      summary,
      content,
      // 添加其他需要的属性
    };

    // 调用保存回调函数
    onSave(newEntry);

    // 清空输入框
    setName('');
    setSummary('');
    setContent('');
  };

  return (
    <Modal open={isOpen} onClose={onClose}>
      <Box className="modal-container">
        <div className="top-actions-container">
          <IconButton aria-label="close" onClick={onClose}>
            <CloseIcon />
          </IconButton>
        </div>
        <div className="content-container">
          <TextField
            label="Name"
            value={name}
            onChange={(e) => setName(e.target.value)}
            fullWidth
            margin="normal"
          />
          <TextField
            label="Summary"
            value={summary}
            onChange={(e) => setSummary(e.target.value)}
            fullWidth
            margin="normal"
          />
          <Select
            value={contentType}
            onChange={(e) => setContentType(e.target.value)}
            displayEmpty
            fullWidth
            margin="normal"
          >
            <MenuItem value="" disabled>
              Select Content Type
            </MenuItem>
            <MenuItem value="text/org">text/org</MenuItem>
            <MenuItem value="text/md">text/md</MenuItem>
          </Select>
          <TextField
            label="Content"
            value={content}
            onChange={(e) => setContent(e.target.value)}
            fullWidth
            margin="normal"
            multiline
            rows={4}
          />
        </div>
        <div className="create-button-wrapper">
          <Button variant="text" onClick={onClose}>
            Cancel
          </Button>
          <Button className="save-btn" variant="contained" onClick={handleSave}>
            Save
          </Button>
        </div>
      </Box>
    </Modal>
  );
};

export default NewEntryModal;
