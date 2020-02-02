import React, {useState} from 'react';
import {Opened, Closed} from '../Icon';
import {Link} from '../Link';

const TreeNode = ({
  className = '',
  setCollapsed,
  collapsed,
  url,
  title,
  items,
}) => {
  const isCollapsed = collapsed[url];
  const collapse = () => {
    setCollapsed(url);
  };
  const hasChildren = items.length !== 0;
  let location;
  if (typeof document != 'undefined') {
    location = document.location;
  }
  const active = location.pathname === url;
  return (
    <li className={`${className} item ${active ? 'active' : ''}`}>
      {title && (
        <Link to={url}>
          {title}
          {hasChildren ? (
            <button
              onClick={collapse}
              aria-label="collapse"
              className="collapser"
            >
              {!isCollapsed ? <Opened /> : <Closed />}
            </button>
          ) : null}
        </Link>
      )}

      {!isCollapsed && hasChildren ? (
        <ul>
          {items.map(item => (
            <TreeNode
              key={item.url}
              setCollapsed={setCollapsed}
              collapsed={collapsed}
              {...item}
            />
          ))}
        </ul>
      ) : null}
    </li>
  );
};

const calculateTreeData = edges => {
  const originalData = edges;
  const tree = originalData.reduce((accu, {node: {fields: {slug, title}}}) => {
    const parts = slug.split('/');
    let {items: prevItems} = accu;
    for (const part of parts.slice(1, -1)) {
      let tmp = prevItems.find(({label}) => label == part);
      if (tmp) {
        if (!tmp.items) {
          tmp.items = [];
        }
      } else {
        tmp = {label: part, items: []};
        prevItems.push(tmp)
      }
      prevItems = tmp.items;
    }
    const existingItem = prevItems.find(({label}) => label === parts[parts.length - 1]);
    if (existingItem) {
      existingItem.url = slug;
      existingItem.title = title;
    } else {
      prevItems.push({
        label: parts[parts.length - 1],
        url: slug,
        items: [],
        title
      });
    }
    return accu;
  }, {items: []});
  return [].reduce((accu, slug) => {
    const parts = slug.split('/');
    let {items: prevItems} = accu;
    for (const part of parts.slice(1, -1)) {
      let tmp = prevItems.find(({label}) => label == part);
      if (tmp) {
        if (!tmp.items) {
          tmp.items = [];
        }
      } else {
        tmp = {label: part, items: []};
        prevItems.push(tmp)
      }
      prevItems = tmp.items;
    }

    // sort items alphabetically.
    prevItems.map((item) => {
      item.items = item.items
        .sort(function (a, b) {
          if (a.label < b.label)
            return -1;
          if (a.label > b.label)
            return 1;
          return 0;
        });
    })
    const index = prevItems.findIndex(({label}) => label === parts[parts.length - 1]);
    accu.items.unshift(prevItems.splice(index, 1)[0]);
    return accu;
  }, tree);
}

export const Tree = ({edges}) => {
  const treeData = calculateTreeData(edges);
  const [collapsed, setCollapsed] = useState({});
  const toggle = (url) => {
    setCollapsed({
      ...collapsed,
      [url]: !collapsed[url],
    });
  }
  return (
    <TreeNode
      className={'showFrontLine firstLevel'}
      setCollapsed={toggle}
      collapsed={collapsed}
      {...treeData}
    />
  );
}
