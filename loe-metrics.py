#! /usr/bin/env python

# WORK-IN-PROGRESS: experiment w/ document-pair LOE measures
# FINISH: similarity model: check satisfaction of triangle inequality

### Name two ordinary text files as command-line parameters.
### These files are assumed to be different representations or
### transformations of the same model.
### For example:
###   FILE1: informal model; FILE2: formal model
###   FILE1: formal model; FILE2: executable model
###   FILE1: informal model; FILE2: executable model

import sys
import math
import optparse
import gensim

## Parse command line
usage = "Usage: %prog [Options] FILE1 FILE2"
op = optparse.OptionParser(usage=usage)
op.add_option("-t", "--tokens", action="store_true", dest="show_tokens",
		default=False, help="display tokens and frequencies")
(options, args) = op.parse_args()

## Check content
def check_content(fp):
	try:
		f = open(fp, "r")
		result = f.read()
	except UnicodeDecodeError:
		print("{}: content not UTF-8".format(fp))
		exit(1)
	f.close

## Preflight check
if len(args) != 2:
	op.print_help()
	exit(1)
fp1 = args[0]
fp2 = args[1]
check_content(fp1)
check_content(fp2)

## Compare similiarity
corpus = []
f = open(fp1, "r")
corpus.append(gensim.utils.simple_preprocess(f.read(), min_len=1))
f.close()
dictionary = gensim.corpora.Dictionary(corpus)
bow_corpus = [dictionary.doc2bow(text) for text in corpus]
lsi = gensim.models.LsiModel(bow_corpus, id2word=dictionary, num_topics=2)
f = open(fp2, "r")
doc = gensim.utils.simple_preprocess(f.read(), min_len=1)
f.close()
doc_bow = dictionary.doc2bow(doc)
vec_lsi = lsi[doc_bow]

## Calculate FILE1 lexical counts
corpus1 = []
loc1 = 0
f1 = open(fp1, "r")
while (True):
	line = f1.readline()
	if not line:
		break
	loc1 += 1
f1.seek(0)
corpus1.append(gensim.utils.simple_preprocess(f1.read(), min_len=1))
f1.close()
dictionary1 = gensim.corpora.Dictionary(corpus1)

## Calculate FILE2 lexical counts
corpus2 = []
loc2 = 0
f2 = open(fp2, "r")
while (True):
	line = f2.readline()
	if not line:
		break
	loc2 += 1
f2.seek(0)
corpus2.append(gensim.utils.simple_preprocess(f2.read(), min_len=1))
f2.close()
dictionary2 = gensim.corpora.Dictionary(corpus2)

## Show token occurrences
def occurs(dict):
	if not options.show_tokens:
		return
	for token, id in dict.token2id.items():
		print("{}: {}".format(token, dict.cfs[id]))

## Calculate Shannon entropy, token-wise
def shannon(dict):
	len = dict.num_pos
	ent = 0.0
	for token, id in dict.token2id.items():
		freq = dict.cfs[id] / len
		ent = ent + freq / math.log(freq, 2)
	return(-ent)

## Report
# files, tokens and frequencies
print("FILE1: {}".format(fp1))
occurs(dictionary1)
print("FILE2: {}".format(fp2))
occurs(dictionary2)
print("\t{:>8}\t{:>8}".format("FILE1", "FILE2"))
# lines of code
print("LOC:\t{:>8}\t{:>8}".format(loc1, loc2))
# total number of tokens
print("TNT:\t{:>8}\t{:>8}".format(dictionary1.num_pos, dictionary2.num_pos))
# number of unique tokens
print("NUT:\t{:>8}\t{:>8}".format(dictionary1.num_nnz, dictionary2.num_nnz))
# Shannon entropy
print("ENT:\t{:8.5f}\t{:8.5f}".format(shannon(dictionary1), shannon(dictionary2)))
print("1/ENT:\t{:8.5f}\t{:8.5f}".format(1/shannon(dictionary1), 1/shannon(dictionary2)))
# latent similarity index
print(lsi)
print(vec_lsi)
