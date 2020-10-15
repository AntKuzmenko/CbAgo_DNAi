##########################################################################################
# Written by Anastasiya Oguienko, PhD student, Caltech.
# This script is under GPLv3 licensing
##########################################################################################
# The script contains the code used to create FASTA files to buil nucleotide logos in Weblogo tool 
# The scrip was created for paper published in Nature XXX


import Bio
from Bio.Seq import Seq
from Bio.Alphabet import IUPAC
from Bio.SeqRecord import SeqRecord
from Bio import SeqIO
print("imported Bio")

def extract_sequence_strings(fasta_file):
    """Extracts sequences of 2 genomic elements from fasta file into separate variables"""
    genomic_sequence = ''
    plasmid_sequence = ''
    with open(fasta_file, 'r') as file:
        i = 0    
        for line in file:
            if line[0] != '>' and i == 1:
                genomic_sequence += line.strip()
            elif line[0] != '>' and i == 2:
                plasmid_sequence += line.strip()
            else:
                i += 1
    return genomic_sequence, plasmid_sequence

def list_of_fwd_reads(coord_file, sequence):
    """Makes the list of reads from forward chain given the list of starting coordinates (1-based, not 0-based!) and the string with sequence"""
    with open(coord_file, "r") as file:
        reads = []
        for line in file:
            coord = int(line.strip())-1  
            reads.append(Seq(sequence[p-15:p+30])) 
    return reads

def list_of_rev_reads(coord_file, sequence):
    """Makes the list of reads from reverse chain given the list of starting coordinates (1-based, not 0-based!) and lengths of reads, and the string with sequence"""
    with open(coord_file, "r") as file:
        reads = []
        for line in file:
            coord_str, len_str = line.strip('\n').split('\t')
            read_length = int(len_str)
            coord = int(coord_str) + read_length - 2 
            read_reverse_complement = Seq(sequence[p-29:p+16], IUPAC.unambiguous_dna).reverse_complement() 
            reads.append(read_reverse_complement)
    return reads

reference_genome = input('Please, enter the path to fasta file with reference genome')
coord_fwd = input("Please, enter the path to .txt file with start coordinates (1-based) of reads from forward chain")
coord_rev = input("Please, enter the path to .txt file with start coordinates (1-based) and lengths of reads from reverse chain")
new_fasta_file = input('Please, enter the path and the name of a new fasta file to be generated')

# extracting genome and plasmid sequences from the reference genome and writing them to different variables
genome, plasmid = extract_sequence_strings(f'{reference_genome}')
print("Made genome and plasmid strings")

# making the list which contains sequnces of forward reads to analyse
# reads from the plasmid and from the genome have to be counted separately
reads_genome = list_of_fwd_reads(f'{coord_fwd}', genome) 
print("made list of forward reads")

# making the list which contains sequnces of reversed reads to analyse
# reads from the plasmid and from the genome have to be counted separately
reads_reversed_genome = list_of_rev_reads(f'{coord_rev}', genome) 
print("made list of reverse reads")

# making new fatsa file from the lists of reads obtained during previous steps
records = []

for i in reads_genome:
    rec = SeqRecord(i, id='fwd read', description='read')
    records.append(rec)

for i in reads_reversed_genome:
    rec = SeqRecord(i, id='rev read', description='read')
    records.append(rec)
    
print("made SeqRecord object for fasta generation")

# writing sequences to the new FASTA file
with open(f"{new_fasta_file}", "w") as output_handle: #enter the path and the name of a new fasta file to be generated
    SeqIO.write(records, output_handle, "fasta")

print("made fasta file")