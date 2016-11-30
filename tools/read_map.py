#!/usr/bin/env python
'''Reads maps from text files
'''

import sys


def split_rows(source):
    rows = source.splitlines()
    rows = filter(lambda x: bool(x) and not x.isspace(),
                  rows)
    rows = list(rows)
    name = rows[0]
    rows = rows[1:]
    return name, rows


def demograph(source):
    _, rows = split_rows(source)
    def iter_cells():
        for j, col in enumerate(rows):
            for i, c in enumerate(col):
                if not c.isspace():
                    yield (i, j, c)

    cells = list(iter_cells())

    assert_msg = "An odd number of cells is required ({} given)"
    cell_count = len(cells)
    assert cell_count % 2 != 0, assert_msg.format(cell_count)

    return cells


def render_demograph(cells):

    def cell_kv(i, j, c):
        template = "(({}, {}), {})"
        if c.lower() == 'r':
            content = 'Red'
        elif c.lower() == 'b':
            content = 'Blue'
        else:
            msg = "Can't handle character {} at ({}, {})"
            raise ValueError(msg.format(c, i, j))
        return template.format(i, j, content)

    template = "(Dict.fromList [ {} ])"
    return template.format(", ".join(cell_kv(*c) for c in cells))



def districts(source):
    _, rows = split_rows(source)

    def hq_coords():
        for j, col in enumerate(rows):
            for i, c in enumerate(col):
                if c.isupper():
                    yield (i,j)

    districts = [(id_no+1, coord) for id_no, coord
                 in enumerate(sorted(hq_coords()))]
    district_count = len(districts)
    assert_msg = "An odd number of districts is required ({} given)"
    assert district_count % 2 != 0, assert_msg.format(district_count)
    return districts


def render_districts(districts):

    def district_kv(id_no, hq):
        assert id_no != 0, "Districts can't have id=0 (that's the empty district)"
        template = "{{ id = {} , hq = ({}, {}) , assigned = Set.empty }}"
        distr = template.format(id_no, *hq)
        return "({}, {})".format(id_no, distr)

    template = "(Dict.fromList [ {} ])"
    district_source = "\n , ".join(list(district_kv(id_no, hq) for id_no, hq in districts))
    return template.format(district_source)


def bureaugraph(id_no, source):
    try:
        dgraph = demograph(source)
        distrs = districts(source)
    except Exception as e:
        print("Exception during bureaugraph {}".format(id_no))
        print(source)
        raise e

    even_msg = "Uneven cells per district ({} cells, {} districts in bgraph {})"
    assert len(dgraph) % len(distrs) == 0, even_msg.format(
        len(dgraph), len(distrs), id_no)
    fullsize = int(len(dgraph) / len(distrs))

    dgraph_source = render_demograph(dgraph)
    dist_source = render_districts(distrs)
    name, _ = split_rows(source)

    template = "{{ id = {}, name = \"{}\", fullSize = {}, demograph = {}, districts = {} }}"

    return template.format(id_no, name, fullsize, dgraph_source, dist_source)


def game(source):
    game_template = '''module Game exposing(bureaugraphs)

-- Generated file. Do not edit directly.

import Array
import Dict
import Set

import Data exposing(..)

bureaugraphs = (Array.fromList [ {} ])'''
    sources = source.split("\n\n")
    bgraphs = [bureaugraph(i, s) for i,s in enumerate(sources) if s]

    return game_template.format("\n , ".join(bgraphs))




if __name__ == "__main__":
    source = sys.stdin.read()
    output = game(source)
    sys.stdout.write(output)
