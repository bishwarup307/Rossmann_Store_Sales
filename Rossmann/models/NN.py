"""
Created on Sun Oct 25 23:14:02 2015
@author: BISHWARUP
"""
import datetime
import numpy as np
import theano
import theano.tensor as T
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from keras.optimizers import SGD
from keras.regularizers import l1, activity_l2
import xgboost as xgb

ext_cols = ['Store', 'Id', 'Sales', 'split1', 'Date']
feature_names = num_ftr + cat_features + bin_ftr + ext_cols

dt = md[feature_names]
ptr = dt[dt.split1 == 0].copy()
pte = dt[dt.split1 == 2].copy()

split_date = datetime.datetime.strptime('01072015', "%d%m%Y").date()
pth = ptr[ptr.Date >= split_date].copy()
ptr = ptr[ptr.Date < split_date].copy()
ptr = ptr[ptr.Sales != 0]
pth = pth[pth.Sales != 0]

if theano.config.floatX == 'float64':
    epsilon = 1.0e-9
else:
    epsilon = 1.0e-7

def rmspe(y_true, y_pred):
    y_true = T.expm1(y_true)
    y_pred = T.expm1(y_pred)    
    return T.sqrt(T.sqr((y_true - y_pred)/y_true).mean(axis=-1))    
    
ann1m = Sequential()
ann1m.add(Dense(output_dim=300, input_dim=184, init = 'lecun_uniform', activation= 'relu',
                W_regularizer=l1(0.001), activity_regularizer=activity_l2(0.01)))
ann1m.add(Dropout(0.1))
ann1m.add(Dense(output_dim=300, init = 'lecun_uniform', activation='relu'))
ann1m.add(Dropout(0.35))
ann1m.add(Dense(output_dim=300, init = 'lecun_uniform', activation='relu'))
ann1m.add(Dropout(0.35))
ann1m.add(Dense(output_dim=1, init = 'lecun_uniform', activation='linear'))

ann1m.compile(loss = 'mean_squared_error', optimizer= 'Adadelta')

final_features = feature_names[:184]
X_train = np.matrix(ptr[final_features].copy())
y_train = ptr['Sales'].copy()
X_val = np.matrix(pth[final_features].copy())
y_val = pth['Sales'].copy()

ann1m.fit(X_train, y_train, batch_size=150, nb_epoch=250, verbose=1,
          validation_data=(X_val, y_val), shuffle=True, show_accuracy=True)

test_id = np.array(md[md.split1 == 2]['Id'])
X_test = np.matrix(pte[final_features])
preds = ann1m.predict(X_test, verbose = 0)
preds = np.expm1(preds)
preds2 = preds.ravel()

sub = pd.DataFrame({'Id': test_id, 'Sales' : preds2})
sub.to_csv('F:/Kaggle/Rossman/Submissions/Keras_NN_184_300_300_1.csv', index = False)

#####
final_features = feature_names[:185]
X_train = ptr[final_features]
y_train = ptr['Sales']
X_val = pth[final_features]
y_val = pth['Sales']

def ToWeight(y):
    w = np.zeros(y.shape, dtype=float)
    ind = y != 0
    w[ind] = 1./(y[ind]**2)
    return w

def rmspe(yhat, y):
    w = ToWeight(y)
    rmspe = np.sqrt(np.mean( w * (y - yhat)**2 ))
    return rmspe
    
def rmspe_xg(yhat, y):
    # y = y.values
    y = y.get_label()
    y = np.exp(y) - 1
    yhat = np.exp(yhat) - 1
    w = ToWeight(y)
    rmspe = np.sqrt(np.mean(w * (y - yhat)**2))
    return "rmspe", rmspe

dtrain = xgb.DMatrix(X_train, y_train)
dval = xgb.DMatrix(X_val, y_val)

params = {"objective": "reg:linear",
          "eta": 0.009,
          "max_depth": 10,
          "subsample": 0.7,
          "colsample_bytree": 0.7,
          "silent": 1,
          "nthread" : 4
          }
watchlist = [(dval, 'eval'), (dtrain, 'train')]
xgb_hc = xgb.train(params = params,
                   dtrain = dtrain,
                   num_boost_round = 10000,
                   evals = watchlist,
                   early_stopping_rounds= 200,
                   feval = rmspe_xg,
                   verbose_eval=True)
                   
dtest = xgb.DMatrix(pte[final_features])
preds = xgb_hc.predict(dtest)
preds = np.expm1(preds)
subs_xgb = pd.DataFrame({'Id': pte['Id'],
                         'Sales': preds})
                        
subs_xgb.to_csv('F:/Kaggle/Rossman/Submissions/python_xgb_NNdata.csv', index = False)