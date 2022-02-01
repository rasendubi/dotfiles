#!/usr/bin/env python
# coding: utf-8
import json
import os
import pickle
import shutil

from Bio.PDB import PDBIO, PDBParser

with open('ranking_debug.json') as f:
    order = json.load(f)['order']

os.makedirs('backup')

for i in range(5):
    with open(f'result_{order[i]}.pkl', 'rb') as f:
        plddt = pickle.load(f)['plddt']
    for model_fn in [f'ranked_{i}.pdb', f'relaxed_{order[i]}.pdb', f'unrelaxed_{order[i]}.pdb']:
        structure = PDBParser().get_structure('model', model_fn)
        assert len(list(structure.get_residues())) == len(plddt)
        for res_i, res in enumerate(structure.get_residues()):
            for atom in res.get_atoms():
                atom.set_bfactor(plddt[res_i])
        # now save model
        io = PDBIO()
        io.set_structure(structure)
        shutil.copy(model_fn, f'backup/{model_fn}')
        io.save(model_fn)

