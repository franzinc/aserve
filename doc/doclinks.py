#!/usr/bin/scl enable rh-python36 -- python3

import re

SYMBOL_RE = (
    # link declaration
    r'<span id="([\-a-z]+?)"></span>'
    # optional whitespaces/Markdown header
    r'\s*(?:#+ )?'
    # start optional emphasis
    r'(?:\*\*)?'
    # start symbol group
    r'(?:'
    # <package>:[<symbol>] or
    r'(?:`(?:[\.\-a-z]+?):([*\-a-z]+?)`)|'
    # [<package>::<internal-symbol>] or
    r'(?:`((?:[\.\-a-z]+?)::(?:[*\-a-z]+?))`)|'
    # ([<symbol>] ...)
    r'(?:`\(([\-a-z]+?)\s+(?:.+?)\)`)'
    # end symbol group
    r')'
    # end optional emphasis
    r'(?:\*\*)?'
)

SYMBOL_MATCHER = re.compile(SYMBOL_RE,  re.MULTILINE|re.DOTALL)

def generate_links(filenames, verbose=False):
    if verbose:
        print('| {:33} | {:33} | {:33} |'.format(
            'Exported', 'Unexported', 'Defined by example'))
        print(('|' + 35*'-')*3 + '|')
    links = {}
    for filename in filenames:
        with open(filename, 'r') as f:
            for match in re.findall(SYMBOL_MATCHER, f.read()):
                name, exported, internal, form = match[0:4]
                if verbose:
                    print('| {:33} | {:33} | {:33} |'.format(
                        exported, internal, form))
                if exported:
                    symbol = exported
                elif internal:
                    symbol = internal
                else:
                    symbol = form
                links['**`' + symbol + '`**'] = filename + '#' + name
    return links

def rewrite_links(filenames, linkmap, rewrite=False):
    for filename in filenames:
        with open(filename, 'r') as f:
            contents = f.read()
        for symbol, link in linkmap.items():
            # Normalize existing links.
            contents = re.sub(
                r'\[{}\]\(.*?\)'.format(re.escape(symbol)),
                r'[{}]({})'.format(symbol, link),
                contents,
                0,
                re.MULTILINE|re.DOTALL
            )
            # Links non-linked symbols.
            contents = re.sub(
                r'([^\[]){}([^\]])'.format(re.escape(symbol)),
                r'\1[{}]({})\2'.format(symbol, link),
                contents
            )
        if rewrite:
            out_filename = filename
        else:
            out_filename = 'new_' + filename
        with open(out_filename, 'w') as f:
            f.write(contents)

import argparse

if __name__ == '__main__':
    filenames = ['aserve.md', 'tutorial.md', 'htmlgen.md']
    argparser = argparse.ArgumentParser()
    argparser.add_argument(
        '--dry',
        help=('Show collected links & write new files. Otherwise, '
              'links are not shown and files are modified in-place.'),
        action='store_true'
    )
    args = argparser.parse_args()
    linkmap = generate_links(filenames, args.dry)
    rewrite_links(filenames, linkmap, not args.dry)
