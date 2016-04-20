import pandas as pd
import numpy as np
from __future__ import division
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.svm import SVR
from sklearn.cross_validation import train_test_split

ptr = pd.read_csv("F:/Kaggle/Rossman/Blends/Stacking/st1/ptr_fromxgbV6.csv")
pth = pd.read_csv("F:/Kaggle/Rossman/Blends/Stacking/st1/pth_fromxgbV6.csv")
pte = pd.read_csv("F:/Kaggle/Rossman/Blends/Stacking/st1/pte_fromxgbV6.csv")
ID = pd.read_csv("F:/Kaggle/Rossman/Data/test_id.csv")

ptr.fillna(-1, inplace = True)
pth.fillna(-1, inplace = True)
pte.fillna(-1, inplace = True)

Sales = list(ptr.Sales)
cols = list(ptr.columns.values)
feature_names = [x for x in cols if x not in ['Sales', 'Date', 'split1']]

x_train, x_val, y_train, y_val = train_test_split(ptr[feature_names], Sales, test_size = 0.1, random_state = 12)

# Random Forest #
clf_RF = RandomForestRegressor(n_estimators = 1000,
                             max_features = 0.75,
                             max_depth = 8,
                             min_samples_split = 12,
                             oob_score = True,
                             n_jobs = -1,
                             random_state = 737,
                             verbose = 2)
                             
clf_RF = clf_RF.fit(x_train, y_train)

preds_h = clf_RF.predict(pth[feature_names])
RF_holdout = pd.DataFrame({'Date':pth.Date, 'Dow':pth.DayOfWeek,  'Actual':np.exp(pth.Sales)-1, 'Predicted':np.exp(preds_h)-1})
RF_holdout.to_csv("F:/Kaggle/Rossman/Blends/Stacking/RF_holdout.csv", index = False)

preds_RF_py = np.exp(clf_RF.predict(pte[feature_names]))-1
RF_py_sub = pd.DataFrame({'Id':ID.Id, 'Sales':preds_RF_py})
RF_py_sub.to_csv("F:/Kaggle/Rossman/Blends/Stacking/RF_subs.csv", index = False)

# Extreemly Randomized Trees #
reg_ET = ExtraTreesRegressor(n_estimators = 1000,
                             max_features = 0.75,
                             max_depth = 8,
                             min_samples_split = 12,
                             n_jobs = -1,
                             random_state = 737,
                             verbose = 2)

reg_ET = reg_ET.fit(x_train, y_train)

preds_h = reg_ET.predict(pth[feature_names])
ET_holdout = pd.DataFrame({'Date':pth.Date, 'Dow':pth.DayOfWeek,  'Actual':np.exp(pth.Sales)-1, 'Predicted':np.exp(preds_h)-1})
ET_holdout.to_csv("F:/Kaggle/Rossman/Blends/Stacking/ET_holdout.csv", index = False)

preds_ET = np.exp(reg_ET.predict(pte[feature_names]))-1
ET_sub = pd.DataFrame({'Id':ID.Id, 'Sales':preds_ET})
ET_sub.to_csv("F:/Kaggle/Rossman/Blends/Stacking/ET_subs.csv", index = False)

# SVR with RBF kernel #
svr_rbf = SVR(kernel = 'rbf',
              C = 1e4,
              gamma = 0.05,
              epsilon = 0.03,
              max_iter = 10000)

svr_rbf = svr_rbf.fit(x_train, y_train)

preds_h = svr_rbf.predict(pth[feature_names])


######################################################################################
def rmspe(pred, act):
    df = pd.DataFrame({'pred':pred, 'act':act})
    df = df[df.act != 0]
    y = np.array(df.act)
    yhat = np.array(df.pred)
    n = y.shape
    rmspe = np.sqrt(sum(((y-yhat)/y)**2)/n)
    return rmspe
    