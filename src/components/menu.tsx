import React from 'react'
import { DefaultMenuStructure, MenuRoot } from '../utils/menu-structure'

export default function Menu() {
  return (
    <div className="garden-menu">
      <MenuRoot menu={DefaultMenuStructure('main')} />
    </div>
  )
}
