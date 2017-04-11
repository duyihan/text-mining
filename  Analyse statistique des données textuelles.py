# Text mining------TP1

from string import punctuation
from collections import Counter
import matplotlib.pyplot as plt


#read the text
documentName = 'Romeo and Juliet.txt'

document = open(documentName).read()


def stat(document):
    words = document.translate(None, punctuation).lower().split()
    word_counts = Counter(words)
    Hapax = 0
    for item in word_counts.most_common(len(word_counts)):
        if(item[1]==1):
            Hapax+=1

    print("Occurrences:",len(words))
    print("Formes:",len(word_counts))
    print("Hapax:",Hapax)
    print("Fmax:",word_counts.most_common(1)[0][1])
    
#Index du corpus
def indexDuCorpus(document):

    words = document.translate(None, punctuation).lower().split()
    word_counts = Counter(words)
    for item in word_counts.most_common(5):
        ref = []
        for i in range(len(words)):
            if(words[i]==item[0]):
                ref.append(i) 
        print 'Freq:',item[1],'; Forme:',item[0],';references:',ref[1],ref[2],ref[3],ref[4],ref[5],'...'


#Loi de Zipf
def plotLoi(document):
    words = document.translate(None, punctuation).lower().split()
    word_counts = Counter(words)
    x = range(len(word_counts))
    y = range(len(word_counts))
    i = 0
    for item in word_counts.most_common(len(word_counts)):
        y[i] = item[1]
        i+=1
    plt.loglog(x,y,'+r')
    plt.title('Loi de Zipf')
    plt.xlabel('Rang du mot')
    plt.ylabel('Frequence du mot')
    plt.show()


#Call functions
stat(document)
indexDuCorpus(document)
plotLoi(document)


