# George J. Stigler Center for the Study of the Economy and the State -- Visual Identity Guide

## About
The materials contained in this repo represent the basis for the Stigler Center's Visual Identity Guide.

The goal of this design system is to bring greater consistency to the visual materials produced by the Center. As many of the Center's (and affiliated individuals')  research products are often (best) disseminated in visual formats, particularly data visualization, developing a system for improving the recognizability of these visualizations as distinctly "Stigler" is important.

Wherever possible, this design system tries to adhere to the Booth School of Business's visual style guide. However, that guide is inappropriate for data visualization in various ways so this system diverges in some important ways.

In addition to developing a consistent design system, this repo contains templates for data visualization packages that are commonly used in quantitative social sciences: `seaborn`, `ggplot`, and `stata`. The design system is useless if it is not used. 

### About Us:
- [Our website](https://www.chicagobooth.edu/research/stigler)
- [Capitalisn't](https://www.capitalisnt.com/) -- The Stigler Center's Podcast
- [ProMarket](https://www.promarket.org/) --  A publication of the Stigler Center
- [The University of Chicago's Booth School of Business](https://www.chicagobooth.edu)


## Sizing and Rendering
### Platform
The majority of charts and figures produced by the Stigler Center are intended for digital publication (first). Even figures that appear in journal articles will mostly be seen mostly on screens. Consequently we mostly presume pixel-based measurements (biased toward publication/embedding in HTML).

Journals and other publications may have their own requirements for formatting and rendering. Note standard printers print at a resolution of 300 dots per inch (DPI). Consequently, it may be necessary to significantly increase the size of plots intended for printing to reduce aliasing. 

## Typefaces

The preferred typeface of the University of Chicago's Booth School of Business is Trade Gothic LT Standard. This font is freely available to download and is bundled with some of the versions of the themes included in this repo. If you wish to have direct access to the `.otf` files they are available [here](https://fontsgeek.com/trade-gothic-lt-std-font) (and on the Wayback Machine).


## Color Palettes

Because this system and this documentation are largely digital-first, color values are given in hexadecimal, RGB, and HSL. CMYK conversion tools are readily available.


- [x] Core discrete
  - [ ] Do I still need to identify accent colors? Probably a yellow or a purple?
- [ ] Spectral (Brewer)
- [ ] Special
  - [ ] US Partisan


## Thinking about Data Visualization and Charts

**Correlation:**
- Scatterplot
- Connected scatterplot
- Bubble
- XY heatmap

**Ranking:**
- Ordered bar/column
- Slope
- Lollipop
- Bump

**Distribution:**
- Histogram/KDE
- Barbell
- Barcode
- Box-plot/Violin
- Lorenz curve

**Change over Time:**
- Line
- Column
- Slope
- Area
- Candlestick
- Fan (forecasting)
- Connected scatterplot
- Celendar heatmap
- Gantt chart/Priestly timeline
- Circle Timeline


**Magnitude:**
- (Grouped) Column/bar
- Marimekko
- Lollipop
- Bullet

**Part of a Whole:**
- Stacked column/bar
- Treemap
- Marimekko
- Arc/Gridplot
- Venn Diagram
- Waterfall

**Spatial:**
- Choropleth
- Proportional symbol
- Contour map
- Equalised cartogram
- Scaled cartogram
- Dot density
- Heat map

**Flows:**
- Sankey/alluvial
- Watefalll
- Chord


## Resources and materials for others
