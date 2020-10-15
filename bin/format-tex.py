#!/usr/bin/env python
'''
This is a support script for my org mode export. I need it because my elisp skills suck too much..

- generates pngs from svgs
- generates ai (i.e. pdf) files from svgs (for sharing with illustrator people))
- converts tex document to be convertible by pandoc
'''

import argparse
import re
import subprocess

parser = argparse.ArgumentParser(description='Postprocess/fix org mode tex output')
parser.add_argument('--dont-replace-fig-labels', action='store_true')
parser.add_argument('--dont-delete-file-output', action='store_true')
parser.add_argument('--dont-replace-includesvg', action='store_true')
parser.add_argument('infile', type=str)
parser.add_argument('outfile', type=str)
args = parser.parse_args()

with open(args.infile, 'r') as inf:
    content = inf.read()

if not args.dont_delete_file_output:
    content = re.sub(r'\[\[file:\\# Out\[[0-9]*?\]:(.*?)\]\]',
                    r'\1',
                    content,
                    flags=re.MULTILINE | re.DOTALL)

if not args.dont_replace_includesvg:
    for match in re.finditer(r'\\includesvg.*\{(.*)\}', content):
        filename = match.groups()[0]
        subprocess.call(['inkscape', f'--export-png={filename}.png', '--export-dpi=180', f'{filename}.svg'])
        # subprocess.call(['inkscape', '--export-text-to-path', f'--export-pdf={filename}.ai', f'{filename}.svg']) I'll use an illustrator script to convert to .ai

    content = content.replace('includesvg', 'includegraphics')

# now rename the figures
if not args.dont_replace_fig_labels:
    for i, label_match in enumerate(re.finditer(r'\\label\{(fig:.*?)\}', content)):
        label = label_match.groups()[0]
        content = content.replace(f'\\label{{{label}}}', f'Figure {i+1}: ')
        content = content.replace(f'\\ref{{{label}}}', f'{i+1}')

with open(args.outfile, 'w') as outf:
    outf.write(content)
