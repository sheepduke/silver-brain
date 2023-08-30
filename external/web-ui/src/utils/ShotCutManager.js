import React, { useEffect } from 'react';

const ShortcutManager = ({ shortcuts }) => {
  useEffect(() => {
    const handleKeyDown = (event) => {
      // 遍历配置的快捷键映射
      for (const shortcut of shortcuts) {
        if (isShortcutMatch(event, shortcut.keys)) {
          // 执行对应的操作
          shortcut.action();
          break;
        }
      }
    };

    // 监听键盘事件
    window.addEventListener('keydown', handleKeyDown);

    // 清除事件监听
    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [shortcuts]); // 当快捷键配置发生变化时重新绑定事件

  const isShortcutMatch = (event, keys) => {
    // 检查按下的键位是否和快捷键匹配
    for (const key of keys) {
      if (event.key !== key && !event[key + 'Key']) {
        return false;
      }
    }
    return true;
  };

  return null; // 空元素，不渲染任何内容
};

export default ShortcutManager;
