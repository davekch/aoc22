import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
import json
import argparse

parser = argparse.ArgumentParser("""
in ghci do
> parsed <- parse <$> getInput
or
> let parsed = parse testinput
> generateJSON "output.json" parsed
then
$ python visualize.py output.json --outer --inner
""")
parser.add_argument("shapes")
parser.add_argument("--full", action="store_true", help="show full shape")
parser.add_argument("--outer", action="store_true", help="show the outer surface of the shape")
parser.add_argument("--inner", action="store_true", help="show cavities")
args = parser.parse_args()
if not args.full and not args.outer and not args.inner:
    print("use flags --full, --outer, --inner to show shapes")
    exit()


def to_poly(shape: list, **kwargs) -> Poly3DCollection:
    # the order of the points is not right in the original output
    surfaces = [[surface[0], surface[2], surface[3], surface[1]] for surface in shape]
    return Poly3DCollection(surfaces, **kwargs)


with open(args.shapes) as f:
    shapes = json.load(f)

full = shapes["fullshape"]
outer = shapes["outershape"]
inner = shapes["innershape"]


minx = min(p[0] for surface in full for p in surface)
maxx = max(p[0] for surface in full for p in surface)
miny = min(p[1] for surface in full for p in surface)
maxy = max(p[1] for surface in full for p in surface)
minz = min(p[2] for surface in full for p in surface)
maxz = max(p[2] for surface in full for p in surface)

full_faces = to_poly(
    full,
    alpha=0.5,
    facecolor="green",
    edgecolor="black",
)

inner_faces = to_poly(
    inner,
    alpha=0.6,
    facecolor="red",
    edgecolor="black",
)

outer_faces = to_poly(
    outer,
    alpha=0.3,
    edgecolor="black",
)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

if args.full:
    ax.add_collection3d(full_faces)
if args.outer:
    ax.add_collection3d(outer_faces)
if args.inner:
    ax.add_collection3d(inner_faces)

ax.set_xlim(minx, maxx)
ax.set_ylim(miny, maxy)
ax.set_zlim(minz, maxz)
ax.axes.set_aspect("equal")
plt.show()
