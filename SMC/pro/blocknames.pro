function blocknames, map

  ;; Pull block names out of a map structure; return as a LIST
  
  firsts = map[where(strpos(map.name, '_1.fts') ne -1)].name
  blocks = list(firsts.extract('b[0-9]+'), /extract)

;  blocks = list(file_basename(, '_1.fts'), /extract)

  ;; TODO: add a unique optional keyword?
  
  return, blocks
end
