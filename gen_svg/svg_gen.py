
svg = '<!DOCTYPE html><html><body>'

width = 1700

svg += '<svg height="210" width="{}">'.format(width)
svg += '<polygon points="{},{} {},{} {},{} {},{}" style="fill:#883388;stroke:purple;stroke-width:1"/>'.format(0,0,width,0,width,100,0,100)
svg += '</svg>'




svg += '</body></html>'
f = open("svg.html", "w")
f.write(svg)
f.close()