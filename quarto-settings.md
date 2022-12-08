## YML

```yml
---
title: ""
author: ""
date: "`r Sys.Date() |> format('%D')`"
editor: source
format: 
  html:
    code-tools: true # show </> source code
    page-layout: full
    anchor-sections: true
    toc: true
execute:
  echo: false
  message: false
  warning: false
  fig.align: "center"
  fig.width: 12
  fig.height: 4
  fig.retina: 3
  out.width: "100%"
---
```

## CSS

To increase width
```css
<style>
@media(min-width: 1300px) {
 body .page-columns {
  display:grid;
  gap:0;
  grid-template-columns:
    [screen-start] 1.5em 
    [screen-start-inset] 5fr 
    [page-start page-start-inset] 35px 
    [body-start-outset] 35px 
    [body-start] 1.5em 
    [body-content-start] minmax(500px, calc(1300px - 3em)) 
    [body-content-end] 1.5em [body-end] 35px 
    [body-end-outset] minmax(75px, 145px) 
    [page-end-inset] 35px [page-end] 5fr 
    [screen-end-inset] 1.5em 
    [screen-end]
 }
}
</style>
```
