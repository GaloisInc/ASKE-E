#! /usr/bin/env python

# WORK-IN-PROGRESS: experiment w/ document-pair LOE measures
# FINISH: similarity model: check satisfaction of triangle inequality

### Name two ordinary text files as command-line parameters.
### These files are assumed to be different representations/transformations of the same model.
### For example:
###   FILE1: informal model; FILE2: formal model
###   FILE1: formal model; FILE2: executable model
###   FILE1: informal model; FILE2: executable model

import sys
import gensim

## Checked read
def check_file(fp):
	try:
		f = open(fp, "r")
		result = f.read()
	except UnicodeDecodeError:
		print("{}: content not UTF-8".format(fp))
		exit(1)
	f.close

## Preflight check
if len(sys.argv) != 3:
	print("usage: {} FILE1 FILE2".format(sys.argv[0]))
	exit(1)
fp1 = sys.argv[1]
fp2 = sys.argv[2]
check_file(fp1)
check_file(fp2)

## Compare similiarity
corpus = []
f = open(fp1, "r")
corpus.append(gensim.utils.simple_preprocess(f.read()))
f.close()
dictionary = gensim.corpora.Dictionary(corpus)
bow_corpus = [dictionary.doc2bow(text) for text in corpus]
lsi = gensim.models.LsiModel(bow_corpus, id2word=dictionary, num_topics=2)
f = open(fp2, "r")
doc = gensim.utils.simple_preprocess(f.read())
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
corpus1.append(gensim.utils.simple_preprocess(f1.read()))
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
corpus2.append(gensim.utils.simple_preprocess(f2.read()))
f2.close()
dictionary2 = gensim.corpora.Dictionary(corpus2)

## Report
# files, tokens and frequencies
print("FILE1: {}".format(fp1))
print(dictionary1.token2id)
print(dictionary1.cfs)
print("FILE2: {}".format(fp2))
print(dictionary2.token2id)
print(dictionary2.cfs)
print("\t{:>8}\t{:>8}".format("FILE1", "FILE2"))
# lines of code
print("LOC:\t{:>8}\t{:>8}".format(loc1, loc2))
# total number of tokens
print("TNT:\t{:>8}\t{:>8}".format(dictionary1.num_pos, dictionary2.num_pos))
# number of unique tokens
print("NUT:\t{:>8}\t{:>8}".format(dictionary1.num_nnz, dictionary2.num_nnz))
# latent similarity index
print(lsi)
print(vec_lsi)
