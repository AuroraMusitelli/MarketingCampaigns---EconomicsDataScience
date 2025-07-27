import telebot

import pickle
modello = open('modello.sav','rb')
best_model=pickle.load(modello)
vect=open('vectorizer.sav','rb')
vectorizer=pickle.load(vect)
data=open('db.sav', 'rb')
db=pickle.load(data)

from gensim.models import FastText
from gensim.test.utils import get_tmpfile
ft_model = FastText.load('ft_model_gensim4.vec')

import pandas as pd
df=pd.read_csv('tcc_ceds_music.csv')
df.drop(index=[18097, 18885, 20580, 20800], inplace=True)



from scipy import spatial
tree = spatial.KDTree(db)

API_KEY='5544998344:AAFH_MTpdLotwElq7FG89kK1cSpBghSkCaw'
bot = telebot.TeleBot(API_KEY)

@bot.message_handler(commands=['start'])
def greet(message):
  bot.reply_to(message, "Hi! This is MoodUp chat bot. Type at least five words separated by a space describing the song you'd like to listen to and I will suggest three songs you might like (for example: Joy happy peace flowers smile). Text me whenever you need an advice 🤗")

def testo_canzone(message):
  request = message.text.split()
  if len(request) < 5:
    return False
  else:
    return True

@bot.message_handler(func=testo_canzone)
def consiglia_canzone(message):
    testo=message.text.split()
    testo1=testo.copy()
    for word in testo:
      try:
        sim=ft_model.wv.most_similar(word)
        for el in sim:
          testo1.append(el[0])
      except KeyError:
        pass
    testo2=' '.join(testo1)
    li=[testo2]
    v=vectorizer.transform(li)
    v1=best_model.predict_proba(v)
    ind1=tree.query(v1, k=3)[1]
    ind=ind1[0]
    s1=ind[0]
    s2=ind[1]
    s3=ind[2]
    titolo1=df.loc[s1, 'track_name']
    artista1=df.loc[s1, 'artist_name']
    titolo2=df.loc[s2, 'track_name']
    artista2=df.loc[s2, 'artist_name']
    titolo3=df.loc[s3, 'track_name']
    artista3=df.loc[s3, 'artist_name']
    response='First song: '+titolo1+' - '+artista1+'\n'+'Second song: ' + titolo2 + ' - ' + artista2 + '\n' + 'Third song: ' + titolo3 + ' - ' + artista3
    bot.send_message(message.chat.id, response)

bot.polling()

