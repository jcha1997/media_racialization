#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Full attribution to Rodman (2020) for the original code. 
"""

import glob
import re
import gensim
import copy
import numpy as np
import pandas as pd
from gensim.models import Word2Vec
from sklearn.utils import resample
    
def chrono_train(n_iterations, current_corpus, previous_model, output_model):
    ''' Models the current corpus by initializing with the vectors of  '''
    ''' the previous model, outputs similarity scores of new model,    '''
    ''' and saves that new model for the next round of modeling        '''
    housing = []
    policing = []
    education = []
    # race = []
    # civil_rights = []
    park = []
    for k in range(n_iterations):
        sentence_samples = resample(current_corpus)
        model = Word2Vec.load(previous_model)
        model.train(sentence_samples, total_examples = len(sentence_samples), epochs = model.epochs)
        housing.append(model.wv.similarity('housing','racialization'))
        policing.append(model.wv.similarity('police','racialization'))
        education.append(model.wv.similarity('education','racialization'))
        #race.append(model.wv.similarity('equality','race'))
        #civil_rights.append(model.wv.similarity('equality','african_american'))
        park.append(model.wv.similarity('park_and_recreation','racialization'))
        run = k+1
        print("Finished with run %d out of %d" % (run, n_iterations))
    model.save(output_model)
    stats = housing, policing, park, education
    return list(stats) 
    
def chrono_train_hc(n_iterations, current_corpus, previous_model, output_model):
    ''' Models the current corpus by initializing with the vectors of  '''
    ''' the previous model, outputs similarity scores of new model,    '''
    ''' and saves that new model for the next round of modeling        '''
    racialization = []
    gender = []
    poverty = []
    black = []
    elderly = []

    for k in range(n_iterations):
        sentence_samples = resample(current_corpus)
        model = Word2Vec.load(previous_model)
        model.train(sentence_samples, total_examples = len(sentence_samples), epochs = model.epochs)
        racialization.append(model.wv.similarity('healthcare','racialization'))
        gender.append(model.wv.similarity('healthcare', 'gender'))
        poverty.append(model.wv.similarity('healthcare', 'poverty'))
        black.append(model.wv.similarity('healthcare', 'blck'))
        elderly.append(model.wv.similarity('healthcare', 'elderlyold'))
        run = k+1
        print("Finished with run %d out of %d" % (run, n_iterations))
    model.save(output_model)
    stats = racialization, gender, poverty, black, elderly
    return list(stats) 

def chrono_train_wf(n_iterations, current_corpus, previous_model, output_model):
    ''' Models the current corpus by initializing with the vectors of  '''
    ''' the previous model, outputs similarity scores of new model,    '''
    ''' and saves that new model for the next round of modeling        '''
    welfare = []

    for k in range(n_iterations):
        sentence_samples = resample(current_corpus)
        model = Word2Vec.load(previous_model)
        model.train(sentence_samples, total_examples = len(sentence_samples), epochs = model.epochs)
        welfare.append(model.wv.similarity('welfare','racialization'))
        run = k+1
        print("Finished with run %d out of %d" % (run, n_iterations))
    model.save(output_model)
    stats = welfare
    return list(stats) 
    
## function that takes a directory of .txt files from ProQuest as input 

def parsePQ(path, file_output=''): 
    sep = "____________________________________________________________"

    fieldnames = ['Title', 'Publication title', 'Publication year', 'Document URL', 'Full text', 'Dateline', 'People', 'Business indexing term', 'Company / organization', 'Credit', 'CREDIT', 'Pages', 'Links', 'Section', 'Publication subject', 'ISSN', 'Copyright', 'Abstract', 'Publication info', 'Last updated', 'Place of publication', 'Location', 'Author', 'Publisher', 'Identifier / keyword', 'Source type', 'ProQuest document ID', 'Country of publication', 'Language of publication', 'Publication date', 'Subject', 'Database', 'Document type']
    # create a dataframe with fieldname as column headers 
    df = pd.DataFrame(columns = fieldnames)
    
    #cycle through every text file in the directory given as an argument
    files_all = glob.iglob(path + "*.txt")
        
    for filename in files_all:

        #remove the path, whitespace, and '.txt' from filename to later use when printing output
        file_id = filename[:-4].strip(path)

        with open(filename, 'r', encoding='utf-8') as in_file:
            # text var for string of all docs
            text = in_file.read()

            # split string by separator into single articles
            docs = re.split(sep, text)

            # remove first and last items from docs list: first item is empty string; last is copyright info
            docs = docs[1:-1]

            # loop through every doc to collect metadata and full text
            for i, doc in enumerate(docs):
                if file_output == 'txt':
                    new_file = 'output/' + file_id + str(i) + '.txt'
                    txt_file = open(new_file,'w') 
                # remove white space from beginning and end of each article
                doc = doc.strip()

                # skip any empty docs
                if doc=="":
                    continue
                   
                if file_output == 'txt':
                    txt_file.write(doc) 
                    txt_file.close() 
                    
                # split doc on every new line
                metadata_lines = doc.split('\n\n')

                #remove first "line" from article which is the article title without any field title
                metadata_lines = metadata_lines[1:]

                #declare a new dictionary
                metadata_dict = {}

                #for each element add the fieldname/key and following value to a dictionary
                for line in metadata_lines:
                    
                    #ignore lines that do not have a field beginning "Xxxxxx:" (e.g. "Publication title: ")
                    if not re.match(r'^[^:]+: ', line):
                        continue
                    #looks for beginning of new line following structure of "Publication year: " splitting on the colon space
                    (key,value) = line.split(sep=': ', maxsplit=1)
                    
                    #only add to dictionary if the key is in fieldnames
                    if key in fieldnames:
                        metadata_dict[key] = value
                    
                    #look for full text that was cut off by double line breaks and append it to the full text field
                    else:

                        if 'Full text' in metadata_dict.keys():
                            metadata_dict['Full text'] = metadata_dict['Full text'] + line
                
                #write the article to a row in the dataframe
                df.loc[len(df)] = metadata_dict        
            print("Saving", file_id)
    
    #return dataframe 
    return(df)