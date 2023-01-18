okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#F0E442")

# https://learnui.design/tools/data-color-picker.html#palette
# 
# https://mokole.com/palette.html
six_col = c("#000000", "#2e8b57", "#ffa500", "#0000ff", "#1e90ff", "#ff1493")
seven_col = c(
  "#000000",
  "#66cdaa",
  "#ff8c00",
  "#00ff00",
  "#0000ff",
  "#1e90ff",
  "#ff1493"
)
eight_col = c(
  "#ff0000",
  "#000000",
  "#ffd700",
  "#c71585",
  "#40e0d0",
  "#00ff00",
  "#0000ff",
  "#1e90ff"
)

qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
qual_col = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

apple_dark = c("#2141E2", "#348A36", "#3A35A7", "#C13500", "#CB1242", "#8645AE", "#CF0400", "#2471A7", "#9B5A00")
apple_light = c("#539CFF", "#4FDB4F", "#807BFF", "#F9B328", "#F66581", "#D690FF", "#F66A5C", "#7FD7FF", "#FAD400")

bigsur <- c(
'#F06048',
'#C03078',
'#D81878',
'#F01860',
'#484878',
'#1878C0',
'#3090C0',
'#C0D8F0',
'#F0A848',
'#FF6060',
'#48A8D8',
'#6078A8',
'#60A8F0',
'#A8C0F0',
'#FFA848',
'#F0D8D8',
'#F0C090',
'#786090',
'#7890C0',
'#D86078')

kinh <- c(
  '#303030',
  '#D84860',
  '#3090A8',
  '#C06078',
  '#607890',
  '#604878',
  '#787890',
  '#D8A8A8',
  '#F0D8C0',
  '#F0F0D8'
)

c.dcp <- c(
  "#003f5c",
  "#2f4b7c",
  "#665191",
  "#a05195",
  "#d45087",
  "#f95d6a",
  "#ff7c43",
  "#ffa600"
)

gmap <- c(
  Iron = "#D5D8DB",
  White_Lilac = "#E8E8E8",
  Madang = "#C2ECB2",
  Columbia_Blue = "#AADAFF",
  Drover = "#FDF2AF",
  Orange_Yellow = "#F6CF65"
)
gmap <- unname(gmap)

kg <- c(
"#5643BD",
"#9030DA",
"#E3059E",
"#FF293F",
"#FF6538",
"#FFA127",
"#FFC600",
"#FEFE38",
"#97E221",
"#00BD33",
"#00CCD3",
"#007EBE")

zurich <- c(
grDevices::rgb(182, 206, 229, maxColorValue=255),
grDevices::rgb( 88, 132, 179, maxColorValue=255),
grDevices::rgb(229, 181, 197, maxColorValue=255),
grDevices::rgb(204, 102, 134, maxColorValue=255),
grDevices::rgb(242, 206, 193, maxColorValue=255),
grDevices::rgb(232, 123, 112, maxColorValue=255),
grDevices::rgb(249, 235, 170, maxColorValue=255),
grDevices::rgb(229, 207, 108, maxColorValue=255),
grDevices::rgb(204, 229, 181, maxColorValue=255),
grDevices::rgb(145, 190, 100, maxColorValue=255),
grDevices::rgb(182, 227, 209, maxColorValue=255),
grDevices::rgb( 91, 190, 148, maxColorValue=255)
)
RYB <- c(
Red        = "#FE2713",
Vermilion  = "#FD5308",
Orange     = "#FB9900",
Amber      = "#FABC01",
Yellow     = "#FEFE33",
Chartreuse = "#CFEA2C",
Green      = "#66B032",
Teal       = "#0192CE",
Blue       = "#0146FE",
Violet     = "#3D00A4",
Purple     = "#8601AF",
Magenta = "#A8184A"
)

havard <- c(
"#0b0b09",
"#961b36",
"#64821c",
"#206b87",
"#d96043",
"#eba938",
"#989897",
"#d76340",
"#d4d849",
"#4683a8",
"#d46619",
"#f7deb2",
"#b2dbde",
"#d8e9dc"
)

nord = c(
nord0 = "#2E3440",
nord1 = "#3B4253",
nord2 = "#434C5F",
nord3 = "#4C566B",
nord4 = "#D8DEE9",
nord5 = "#E5E9F0",
nord6 = "#ECEFF4",
nord7 = "#8EBCBB",
nord8 = "#86C0D1",
nord9 = "#80A0C2",
nord10 = "#5D80AE",
nord11 = "#C16069",
nord12 = "#D2876D",
nord14 = "#A2BF8A",
nord13 = "#ECCC87",
nord15 = "#B58DAE")

nord9c = c(
nord7 = "#8EBCBB",
nord8 = "#86C0D1",
nord9 = "#80A0C2",
nord10 = "#5D80AE",
nord11 = "#C16069",
nord12 = "#D2876D",
nord14 = "#A2BF8A",
nord13 = "#ECCC87",
nord15 = "#B58DAE")

nord7b = c(
nord0 = "#2E3440",
nord1 = "#3B4253",
nord2 = "#434C5F",
nord3 = "#4C566B",
nord4 = "#D8DEE9",
nord5 = "#E5E9F0",
nord6 = "#ECEFF4")
nord <- nord[c(4, 8:16)]
# show_colors(nord[c(1:5,10,8,9,7,6)])
nord = nord[c(1:5,10,8,9,7,6)]

solarized = c(
Base03 = "#042029",
Base02 = "#0A2933",
Base01 = "#475B62",
Base00 = "#536870",
Base0 = "#708284",
Base1 = "#819090",
Base2 = "#EAE3CB",
Base3 = "#FCF4DC",
Yellow = "#A57706",
Orange = "#BD3613",
Red = "#D11C24",
Magenta = "#C61C6F",
Violet = "#595AB7",
Blue = "#2176C7",
Cyan = "#259286",
Green = "#738A05")

solarize8b = c(
Base03 = "#042029",
Base02 = "#0A2933",
Base01 = "#475B62",
Base00 = "#536870",
Base0 = "#708284",
Base1 = "#819090",
Base2 = "#EAE3CB",
Base3 = "#FCF4DC")

solarize8c = c(
Yellow = "#A57706",
Orange = "#BD3613",
Red = "#D11C24",
Magenta = "#C61C6F",
Violet = "#595AB7",
Blue = "#2176C7",
Cyan = "#259286",
Green = "#738A05")

who_blue <- "#0098CB"

dvinames <- c(
  Apricot       = "#FBB982",
  Aquamarine    = "#00B5BE",
  Bittersweet   = "#C04F17",
  Black         = "#221E1F",
  Blue          = "#2D2F92",
  BlueGreen     = "#00B3B8",
  BlueViolet    = "#473992",
  BrickRed      = "#B6321C",
  Brown         = "#792500",
  BurntOrange   = "#F7921D",
  CadetBlue     = "#74729A",
  CarnationPink = "#F282B4",
  Cerulean      = "#00A2E3",
  CornflowerBlue= "#41B0E4",
  Cyan          = "#00AEEF",
  Dandelion     = "#FDBC42",
  DarkOrchid    = "#A4538A",
  Emerald       = "#00A99D",
  ForestGreen   = "#009B55",
  Fuchsia       = "#8C368C",
  Goldenrod     = "#FFDF42",
  Gray          = "#949698",
  Green         = "#00A64F",
  GreenYellow   = "#DFE674",
  JungleGreen   = "#00A99A",
  Lavender      = "#F49EC4",
  LimeGreen     = "#8DC73E",
  Magenta       = "#EC008C",
  Mahogany      = "#A9341F",
  Maroon        = "#AF3235",
  Melon         = "#F89E7B",
  MidnightBlue  = "#006795",
  Mulberry      = "#A93C93",
  NavyBlue      = "#006EB8",
  OliveGreen    = "#3C8031",
  Orange        = "#F58137",
  OrangeRed     = "#ED135A",
  Orchid        = "#AF72B0",
  Peach         = "#F7965A",
  Periwinkle    = "#7977B8",
  PineGreen     = "#008B72",
  Plum          = "#92268F",
  ProcessBlue   = "#00B0F0",
  Purple        = "#99479B",
  RawSienna     = "#974006",
  Red           = "#ED1B23",
  RedOrange     = "#F26035",
  RedViolet     = "#A1246B",
  Rhodamine     = "#EF559F",
  RoyalBlue     = "#0071BC",
  RoyalPurple   = "#613F99",
  RubineRed     = "#ED017D",
  Salmon        = "#F69289",
  SeaGreen      = "#3FBC9D",
  Sepia         = "#671800",
  SkyBlue       = "#46C5DD",
  SpringGreen   = "#C6DC67",
  Tan           = "#DA9D76",
  TealBlue      = "#00AEB3",
  Thistle       = "#D883B7",
  Turquoise     = "#00B4CE",
  Violet        = "#58429B",
  VioletRed     = "#EF58A0",
  White         = "#FFFFFF",
  WildStrawberry= "#EE2967",
  Yellow        = "#FFF200",
  YellowGreen   = "#98CC70",
  YellowOrange  = "#FAA21A")
