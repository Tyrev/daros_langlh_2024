# -*- coding: utf-8 -*-
"""
Created on Sat Dec  4 10:26:09 2021

@author: Hernan_Lab
"""

from mlxtend.feature_selection import SequentialFeatureSelector as SFS
from mlxtend.plotting import plot_sequential_feature_selection as plot_sfs
from sklearn.linear_model import LinearRegression
from xgboost import XGBClassifier
import pandas as pd
import numpy as np
import re
import matplotlib.pylab as plt
from sklearn.model_selection import train_test_split
import random
from sklearn.utils import shuffle
import pickle
from sklearn.decomposition import PCA
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import scale

import matplotlib.font_manager
from matplotlib import style
style.use('seaborn') or plt.style.use('seaborn')

from tqdm.notebook import tqdm
from sklearn.model_selection import RepeatedKFold
from sklearn.model_selection import KFold
import xgboost as xgb
from sklearn.metrics import roc_auc_score, roc_curve, f1_score, accuracy_score, recall_score, precision_score, confusion_matrix
import plotly.graph_objects as go
import seaborn as sns

from collections import Counter
import operator


from xgboost import plot_importance


from factor_analyzer import FactorAnalyzer
from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity
from factor_analyzer.factor_analyzer import calculate_kmo

from sklearn.decomposition import FactorAnalysis

import matplotlib.path as mpath
import matplotlib.lines as mlines
import matplotlib.patches as mpatches
from matplotlib.collections import PatchCollection
import scipy.stats as ss

import matplotlib.path as mpath
import matplotlib.lines as mlines
import matplotlib.patches as mpatches
from matplotlib.collections import PatchCollection
from matplotlib.patches import Rectangle


def plot_best_pca_feature_reproducibility_bar_color(df_PC_rep_list, num_for_view, xlim, legend, bar_colors_list, f_size, l_size):
    

    #plt.figure(figsize=(14,num_for_view))
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize= f_size)

    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']

    
    
    df_PC_rep_list[0].T.plot(ax=axes[0,0], kind = "barh", legend = False, title = 'PC1',y = "media", xerr = "std", color=bar_colors)
    axes[0,0].set_xlabel('Loading')
    axes[0,0].set_ylabel('Feature');
    axes[0,0].set_ylim([-0.5,9.5])
    axes[0,0].set_xlim([0, xlim])
    axes[0,0].tick_params(axis='both', which='major', labelsize=l_size)
    axes[0,0].yaxis.label.set_size(l_size + 2)
    axes[0,0].xaxis.label.set_size(l_size + 2)
    axes[0,0].set_title('PC1', fontsize= l_size + 5)

    count = 0
    for i in df_PC_rep_list[0].columns:
      axes[0,0].text(df_PC_rep_list[0].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[0][i][2])) +'%', fontsize = l_size, weight="bold", 
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1




    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[1]
    else:
        bar_colors = ['cornflowerblue']
        

    df_PC_rep_list[1].T.plot(ax=axes[0,1], kind = "barh", legend = False, title = 'PC2',y = "media", xerr = "std", color=bar_colors)
    axes[0,1].set_xlabel('Loading')
    axes[0,1].set_ylabel('Feature');
    axes[0,1].set_ylim([-0.5,9.5])
    axes[0,1].set_xlim([0, xlim])
    axes[0,1].tick_params(axis='both', which='major', labelsize=l_size)
    axes[0,1].yaxis.label.set_size(l_size + 2)
    axes[0,1].xaxis.label.set_size(l_size + 2)
    axes[0,1].set_title('PC2', fontsize= l_size + 5)

    count = 0
    for i in df_PC_rep_list[1].columns:
      axes[0,1].text(df_PC_rep_list[1].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[1][i][2])) +'%', fontsize = l_size, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1


    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[2]
    else:
        bar_colors = ['cornflowerblue']

    df_PC_rep_list[2].T.plot(ax=axes[1,0], kind = "barh", legend = False, title = 'PC3',y = "media", xerr = "std", color=bar_colors)
    axes[1,0].set_xlabel('Loading')
    axes[1,0].set_ylabel('Feature');
    axes[1,0].set_ylim([-0.5,9.5])
    axes[1,0].set_xlim([0, xlim])
    axes[1,0].tick_params(axis='both', which='major', labelsize=l_size)
    axes[1,0].yaxis.label.set_size(l_size + 2)
    axes[1,0].xaxis.label.set_size(l_size + 2)
    axes[1,0].set_title('PC3', fontsize= l_size + 5)

    count = 0
    for i in df_PC_rep_list[2].columns:
      axes[1,0].text(df_PC_rep_list[2].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[2][i][2])) +'%', fontsize = l_size, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1



    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[3]
    else:
        bar_colors = ['cornflowerblue']

    df_PC_rep_list[3].T.plot(ax=axes[1,1], kind = "barh", legend = False, title = 'PC4',y = "media", xerr = "std", color=bar_colors)
    axes[1,1].set_xlabel('Loading')
    axes[1,1].set_ylabel('Feature');
    axes[1,1].set_ylim([-0.5,9.5])
    axes[1,1].set_xlim([0, xlim])
    axes[1,1].tick_params(axis='both', which='major', labelsize=l_size)
    axes[1,1].yaxis.label.set_size(l_size + 2)
    axes[1,1].xaxis.label.set_size(l_size + 2)
    axes[1,1].set_title('PC4', fontsize= l_size + 5)


    count = 0
    for i in df_PC_rep_list[3].columns:
      axes[1,1].text(df_PC_rep_list[3].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[3][i][2])) +'%', fontsize = l_size, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1
        
        
        
    plt.tight_layout(pad=3);

        # Legenda manual
    desp = 0.02

    if(legend):

            plt.text(0.165 + desp, 0.000, '-----------------', fontsize=5,color='coral', bbox=dict(facecolor='coral', edgecolor='coral', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            plt.text(0.2 + desp, 0, 'Context', fontsize=14, transform=plt.gcf().transFigure);

            plt.text(0.34 + desp, 0.000, '-----------------', fontsize=5,color='firebrick', bbox=dict(facecolor='firebrick', edgecolor='firebrick', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            plt.text(0.375 + desp, 0, 'Copying', fontsize=14, transform=plt.gcf().transFigure);


            plt.text(0.47 + desp, 0.000, '-----------------', fontsize=5,color='saddlebrown', bbox=dict(facecolor='saddlebrown', edgecolor='saddlebrown', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            plt.text(0.505 + desp, 0, 'MDisorder', fontsize=14, transform=plt.gcf().transFigure);


            #plt.text(0.57 + desp, 0.000, '-----------------', fontsize=5,color='cornflowerblue', bbox=dict(facecolor='cornflowerblue', edgecolor='cornflowerblue', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            #plt.text(0.605 + desp, 0, 'Mean', fontsize=14, transform=plt.gcf().transFigure);


            #plt.text(0.68 + desp, 0.000, '-----------------', fontsize=5,color='khaki', bbox=dict(facecolor='khaki', edgecolor='khaki', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            #plt.text(0.715 + desp, 0, 'MeanAllFactors', fontsize=14, transform=plt.gcf().transFigure);

            plt.text(0.62 + desp, 0.000, '-----------------', fontsize=5,color='palegreen', bbox=dict(facecolor='palegreen', edgecolor='palegreen', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
            plt.text(0.65 + desp, 0, 'Personality', fontsize=14, transform=plt.gcf().transFigure);







def get_color_for_PCA_rep(PCA_rep, X_cat_code, X_cat_name):
    color_dict = {}
    color_dict['Context'] = 'coral'
    color_dict['Copying'] = 'firebrick'
    color_dict['MDisorder'] = 'saddlebrown'
    color_dict['Mdisorder'] = 'saddlebrown'
    color_dict['Mean'] = 'cornflowerblue'
    color_dict['MeanAllFactors'] = 'khaki'
    color_dict['Personality'] = 'palegreen'
    color_dict['Violence_Committed'] = 'red'
	

    bar_color_total = []

    for j in range(4):
        bar_color_per_PC = []
        for i in list(PCA_rep[j].columns):
            index_code = X_cat_code.index(i)
            cat = X_cat_name[index_code].split('_')[0]
            bar_color_per_PC.append(color_dict[cat])
        bar_color_total.append(bar_color_per_PC)


        
    return bar_color_total



def ID_comprob(X):
    list_ID_X = []

    for subject in range(X.shape[0]): 
        current_ID_RAW = list(X.iloc[subject, :])
        current_ID = []

        for i in current_ID_RAW:
            current_ID.append(i)

        list_ID_X.append(current_ID)



    equal_list_test_X = []
    equal_list_test_index_X = []
    equal_list_test_ij_equal_X = []

    dict_equal_X ={}

    for i in range(X.shape[0]):
        for j in range(X.shape[0]):
            if(i==j):
                continue

            c = list_ID_X[i] == list_ID_X[j]

            if(c == True):

                dict_equal_X[str(i) + '_' + str(j)] = c
                equal_list_test_X.append(list_ID_X[i])
                equal_list_test_index_X.append(i)


    print('Rep:', len(dict_equal_X.keys()))
    for i in equal_list_test_index_X:
        for j in equal_list_test_index_X:
            try:
                if(dict_equal_X[str(i) + '_' + str(j)] == True):
                    equal_list_test_index_X.remove(j)
            except:
                pass

    drop_list = equal_list_test_index_X
    print(drop_list)

    # Eliminar repeticiones
    X = X.drop(drop_list, axis=0)


    list_ID_X_nrep = []

    for subject in range(X.shape[0]): 
        current_ID_RAW = list(X.iloc[subject, :])
        current_ID = []

        for i in current_ID_RAW:
            current_ID.append(i)

        list_ID_X_nrep.append(current_ID)


    # Coprobar que no hay repeticiones luego de eliminar repeticiones

    equal_list_test_X = []
    equal_list_test_index_X = []
    equal_list_test_ij_equal_X = []

    dict_equal_X ={}

    for i in range(X.shape[0]):
        for j in range(X.shape[0]):
            if(i==j):
                  continue

            c = list_ID_X_nrep[i] == list_ID_X_nrep[j]

            if(c == True):
                print('i:',i, '||', 'j:', j, '\t', c, '\t', X.iloc[i, 0], '\t', X.iloc[i, -1])
                dict_equal_X[str(i) + '_' + str(j)] = c

                equal_list_test_X.append(list_ID_X_nrep[i])
                equal_list_test_index_X.append(i)

    print('Rep después:', len(dict_equal_X.keys()))   

    return X


def plot_class_balance(df, df_D, ylim):
    
    plt.figure(figsize=(14,6))

    bar_colors = ['lightsalmon', 'brown']

    plt.subplot(1,2,1)
    plt.title('Grupo de Izquierda')
    df["violencia"].value_counts().sort_index().plot(kind = 'bar', rot=0, color=bar_colors)
    plt.xticks(range(2), ['Sin violencia', 'Con violencia'])
    plt.ylim([0, ylim]) 
    plt.xlabel('Violencia Global')
    plt.ylabel('Cantidad')

    plt.text(0, df["violencia"].value_counts().sort_index()[0], 
             str(df["violencia"].value_counts().sort_index()[0]),
             size=14, rotation=0.,
             ha="center", va="center",
             bbox=dict(boxstyle="round",
                       ec=(255/255, 140/255, 100/255),
                        fc=(255/255, 140/255, 100/255),
                       )
             )

    plt.text(1, df["violencia"].value_counts().sort_index()[1], 
             str(df["violencia"].value_counts().sort_index()[1]),
             size=14, rotation=0.,
             ha="center", va="center",
             bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       )
             )

    plt.subplot(1,2,2)
    plt.title('Grupo de Derecha')
    df_D["violencia"].value_counts().sort_index().plot(kind = 'bar', rot=0,color=bar_colors)
    plt.xticks(range(2), ['Sin violencia', 'Con violencia'])
    plt.ylim([0, ylim])
    plt.xlabel('Violencia Global')
    plt.ylabel('Cantidad');

    plt.text(0, df_D["violencia"].value_counts().sort_index()[0], 
             str(df_D["violencia"].value_counts().sort_index()[0]),
             size=14, rotation=0.,
             ha="center", va="center",
             bbox=dict(boxstyle="round",
                       ec=(255/255, 140/255, 100/255),
                        fc=(255/255, 140/255, 100/255),
                       )
             )

    plt.text(1, df_D["violencia"].value_counts().sort_index()[1], 
             str(df_D["violencia"].value_counts().sort_index()[1]),
             size=14, rotation=0.,
             ha="center", va="center",
             bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       )
             );

    
    
def pca_plot(m_PCA, df, n):
    
    df_PCA  = pd.Series(m_PCA.explained_variance_ratio_[0:n], index = range(1,n+1))

    var_acum  = m_PCA.explained_variance_ratio_.cumsum()

    plt.figure(figsize=(16,6))
    plt.subplot(1,2,1)
    df_PCA .plot(kind = "bar")
    plt.yticks(np.round(np.linspace(0,0.2,9), 3), np.round(np.linspace(0,0.2,9), 3))


    plt.xticks(rotation = 0)
    plt.ylabel('Varianza explicada',fontsize=12,fontweight='bold')
    plt.xlabel('Componentes', fontsize=12,fontweight='bold')
    #plt.title("PCA",fontsize=15,fontweight='bold');


    plt.subplot(1,2,2)
    plt.plot(np.arange(len(df.columns)) + 1, var_acum , label='varianza explicada acumulada')
    plt.axhline(y = 0.95, color='k', linestyle='--', label = '0.95 Varianza explicada')
    plt.ylabel('Varianza explicada acumulada',fontsize=12,fontweight='bold')
    plt.xlabel('Componentes', fontsize=12,fontweight='bold')
    plt.legend(loc='best');

    pc_95 = np.where(var_acum >0.95)[0][0]

    plt.annotate('PC ' + str(pc_95), xy=(pc_95 , 0.95), xytext=(pc_95 + 5, 0.85),
                arrowprops=dict(facecolor='black', shrink=0.05), size=12,
                ha="center", va="center",bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                      )
                );

    return (df_PCA , var_acum )



def plot_best_pca_feature(load, num_for_view, xlim, legend, bar_colors_list):


    PC1_load = np.abs(load.iloc[0,:])
    PC2_load = np.abs(load.iloc[1,:])
    PC3_load = np.abs(load.iloc[2,:])
    PC4_load = np.abs(load.iloc[3,:])


    plt.figure(figsize=(14,num_for_view))


    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']

    plt.subplot(2,2,1)
    PC1_load.sort_values().iloc[-num_for_view::].plot(kind = 'barh', color=bar_colors)
    plt.xlabel('Loading')
    plt.ylabel('Feature');
    plt.xlim([0, xlim])
    plt.title('PC1')



    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[1]
    else:
          bar_colors = ['cornflowerblue']

    plt.subplot(2,2,2)
    PC2_load.sort_values().iloc[-num_for_view::].plot(kind = 'barh', color=bar_colors)
    plt.xlabel('Loading')
    plt.ylabel('Feature');
    plt.xlim([0, xlim])
    plt.title('PC2')


    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[2]
    else:
        bar_colors = ['cornflowerblue']

    plt.subplot(2,2,3)
    PC3_load.sort_values().iloc[-num_for_view::].plot(kind = 'barh', color=bar_colors)
    plt.xlabel('Loading')
    plt.ylabel('Feature');
    plt.xlim([0, xlim])
    plt.title('PC3')


    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[3]
    else:
        bar_colors = ['cornflowerblue']

    plt.subplot(2,2,4)
    PC4_load.sort_values().iloc[-num_for_view::].plot(kind = 'barh', color=bar_colors)
    plt.xlabel('Loading')
    plt.ylabel('Feature');
    plt.title('PC4')
    plt.xlim([0, xlim])
    plt.tight_layout(pad=3);


    # Legenda manual
    
    if(legend):
        desp = 0.09

        plt.text(0.165 + desp, 0.004, '-----------------', fontsize=5,color='coral', bbox=dict(facecolor='coral', edgecolor='coral', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.2 + desp, 0, 'Trastorno Mental', fontsize=14, transform=plt.gcf().transFigure);

        plt.text(0.34 + desp, 0.004, '-----------------', fontsize=5,color='firebrick', bbox=dict(facecolor='firebrick', edgecolor='firebrick', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.375 + desp, 0, 'Educación', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.47 + desp, 0.004, '-----------------', fontsize=5,color='saddlebrown', bbox=dict(facecolor='saddlebrown', edgecolor='saddlebrown', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.505 + desp, 0, 'Salud', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.57 + desp, 0.004, '-----------------', fontsize=5,color='cornflowerblue', bbox=dict(facecolor='cornflowerblue', edgecolor='cornflowerblue', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.605 + desp, 0, 'Política', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.68 + desp, 0.004, '-----------------', fontsize=5,color='darkgray', bbox=dict(facecolor='darkgray', edgecolor='darkgray', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.715 + desp, 0, 'Sin categoría', fontsize=14, transform=plt.gcf().transFigure);
        
        
        
def comnpute_pca_dict(df, n, n_splits = 10):
    
    column_list = []
    for i in range(1,n+1):
        column_list.append('PC' + str(i))


    count =0
    pca_dicts = {}
    kf = KFold(n_splits=10)

    for train, test in kf.split(df):

        pca_pipes = 0
        pca_pipes = make_pipeline(StandardScaler(), PCA(n_components=n))
        pca_pipes.fit(df.iloc[train,:])
        train_pcas = pca_pipes.transform(X=df.iloc[train,:])
        train_pcas = pd.DataFrame(train_pcas, columns = column_list, index = df.iloc[train,:].index)

        pca_dicts[count] = pca_pipes

        count+=1
        
    return pca_dicts


def compute_statistics_reproducibility(df, load, pca_dict):
    
    PC1_load = np.abs(load.iloc[0,:])
    PC2_load = np.abs(load.iloc[1,:])
    PC3_load = np.abs(load.iloc[2,:])
    PC4_load = np.abs(load.iloc[3,:])


    num_for_comp = 10

    d_PC1 = {}
    d_PC2 = {}
    d_PC3 = {}
    d_PC4 = {}

    for i in list(PC1_load.sort_values().iloc[-num_for_comp::].index):
        d_PC1[i] = []

    for i in list(PC2_load.sort_values().iloc[-num_for_comp::].index):
        d_PC2[i] = []

    for i in list(PC3_load.sort_values().iloc[-num_for_comp::].index):
        d_PC3[i] = []

    for i in list(PC4_load.sort_values().iloc[-num_for_comp::].index):
        d_PC4[i] = []
        
        
        
    PC1_rep = []
    PC2_rep = []
    PC3_rep = []
    PC4_rep = []

    for i in range(10):
        m_PCA_ = pca_dict[i].named_steps['pca']
        load_ = pd.DataFrame(data = m_PCA_.components_[0:4], columns = df.columns, index   = ['PC1', 'PC2', 'PC3', 'PC4'])

        PC1_load_ = np.abs(load_.iloc[0,:])
        PC2_load_ = np.abs(load_.iloc[1,:])
        PC3_load_ = np.abs(load_.iloc[2,:])
        PC4_load_ = np.abs(load_.iloc[3,:])


        for j in d_PC1.keys():
            try:
                d_PC1[j].append(PC1_load_.sort_values().iloc[-num_for_comp::][j])
            except:
                pass

        for j in d_PC2.keys():
            try:
                d_PC2[j].append(PC2_load_.sort_values().iloc[-num_for_comp::][j])
            except:
                pass      

        for j in d_PC3.keys():
            try:
                d_PC3[j].append(PC3_load_.sort_values().iloc[-num_for_comp::][j])
            except:
                pass

        for j in d_PC4.keys():
            try:
                d_PC4[j].append(PC4_load_.sort_values().iloc[-num_for_comp::][j])
            except:
                pass

        PC1_rep.extend(list(PC1_load_.sort_values().iloc[-num_for_comp::].index))
        PC2_rep.extend(list(PC2_load_.sort_values().iloc[-num_for_comp::].index))
        PC3_rep.extend(list(PC3_load_.sort_values().iloc[-num_for_comp::].index))
        PC4_rep.extend(list(PC4_load_.sort_values().iloc[-num_for_comp::].index))
        
    d_PC1_m = {}; d_PC1_s = {}; d_PC1_p = {};d_PC1_all = {}
    d_PC2_m = {}; d_PC2_s = {}; d_PC2_p = {};d_PC2_all = {}
    d_PC3_m = {}; d_PC3_s = {}; d_PC3_p = {};d_PC3_all = {}
    d_PC4_m = {}; d_PC4_s = {}; d_PC4_p = {};d_PC4_all = {}

    for j in d_PC1.keys():
        if(np.round(100*len(d_PC1[j])/10,2) == 100):
            d_PC1_all[j] = [np.mean(d_PC1[j]), np.std(d_PC1[j]), np.round(100*len(d_PC1[j])/10,2)]
            d_PC1_m[j] = np.mean(d_PC1[j])
            d_PC1_s[j] = np.std(d_PC1[j])
            d_PC1_p[j] = np.round(100*len(d_PC1[j])/10,2)

    for j in d_PC2.keys():
        if(np.round(100*len(d_PC2[j])/10,2) == 100):
            d_PC2_all[j] = [np.mean(d_PC2[j]), np.std(d_PC2[j]), np.round(100*len(d_PC2[j])/10,2)]
            d_PC2_m[j] = np.mean(d_PC2[j])
            d_PC2_s[j] = np.std(d_PC2[j])
            d_PC2_p[j] = np.round(100*len(d_PC2[j])/10,2)

    for j in d_PC3.keys():
        if(np.round(100*len(d_PC3[j])/10,2) == 100):
            d_PC3_all[j] = [np.mean(d_PC3[j]), np.std(d_PC3[j]), np.round(100*len(d_PC3[j])/10,2)]
            d_PC3_m[j] = np.mean(d_PC3[j])
            d_PC3_s[j] = np.std(d_PC3[j])
            d_PC3_p[j] = np.round(100*len(d_PC3[j])/10,2)

    for j in d_PC4.keys():    
        if(np.round(100*len(d_PC4[j])/10,2) == 100):
            d_PC4_all[j] = [np.mean(d_PC4[j]), np.std(d_PC4[j]), np.round(100*len(d_PC4[j])/10,2)]
            d_PC4_m[j] = np.mean(d_PC4[j])
            d_PC4_s[j] = np.std(d_PC4[j])
            d_PC4_p[j] = np.round(100*len(d_PC4[j])/10,2)


    df_PC1_all = pd.DataFrame(data =d_PC1_all, index = ['media', 'std', 'porc'])
    df_PC2_all = pd.DataFrame(data =d_PC2_all, index = ['media', 'std', 'porc'])
    df_PC3_all = pd.DataFrame(data =d_PC3_all, index = ['media', 'std', 'porc'])
    df_PC4_all = pd.DataFrame(data =d_PC4_all, index = ['media', 'std', 'porc'])
    
    return (df_PC1_all, df_PC2_all, df_PC3_all, df_PC4_all)



def plot_best_pca_feature_reproducibility(df_PC_rep_list, num_for_view, xlim, legend, bar_colors_list):
    

    #plt.figure(figsize=(14,num_for_view))
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(14,num_for_view))

    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']


    df_PC_rep_list[0].T.plot(ax=axes[0,0], kind = "barh", legend = False, title = 'PC1',y = "media", xerr = "std", color=bar_colors)
    axes[0,0].set_xlabel('Loading')
    axes[0,0].set_ylabel('Feature');
    axes[0,0].set_ylim([-0.5,9.5])
    axes[0,0].set_xlim([0, xlim])

    count = 0
    for i in df_PC_rep_list[0].columns:
      axes[0,0].text(df_PC_rep_list[0].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[0][i][2])) +'%', fontsize = 10, weight="bold", 
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1




    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']
        

    df_PC_rep_list[1].T.plot(ax=axes[0,1], kind = "barh", legend = False, title = 'PC2',y = "media", xerr = "std", color=bar_colors)
    axes[0,1].set_xlabel('Loading')
    axes[0,1].set_ylabel('Feature');
    axes[0,1].set_ylim([-0.5,9.5])
    axes[0,1].set_xlim([0, xlim])

    count = 0
    for i in df_PC_rep_list[1].columns:
      axes[0,1].text(df_PC_rep_list[1].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[1][i][2])) +'%', fontsize = 10, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1


    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']

    df_PC_rep_list[2].T.plot(ax=axes[1,0], kind = "barh", legend = False, title = 'PC3',y = "media", xerr = "std", color=bar_colors)
    axes[1,0].set_xlabel('Loading')
    axes[1,0].set_ylabel('Feature');
    axes[1,0].set_ylim([-0.5,9.5])
    axes[1,0].set_xlim([0, xlim])

    count = 0
    for i in df_PC_rep_list[2].columns:
      axes[1,0].text(df_PC_rep_list[2].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[2][i][2])) +'%', fontsize = 10, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1



    if len(bar_colors_list) >0:
        bar_colors = bar_colors_list[0]
    else:
        bar_colors = ['cornflowerblue']

    df_PC_rep_list[3].T.plot(ax=axes[1,1], kind = "barh", legend = False, title = 'PC4',y = "media", xerr = "std", color=bar_colors)
    axes[1,1].set_xlabel('Loading')
    axes[1,1].set_ylabel('Feature');
    axes[1,1].set_ylim([-0.5,9.5])
    axes[1,1].set_xlim([0, xlim])


    count = 0
    for i in df_PC_rep_list[3].columns:
      axes[1,1].text(df_PC_rep_list[3].iloc[0,:].max()/2, count, str(int(df_PC_rep_list[3][i][2])) +'%', fontsize = 10, weight="bold",
            horizontalalignment='center',
            verticalalignment='center', bbox=dict(boxstyle="round",
                       ec=(255/255, 102/255, 90/255),
                        fc=(255/255, 102/255, 90/255),
                       ))
      count+=1

    plt.tight_layout(pad=3);

    # Legenda manual
    desp = 0.09
    
    if(legend):

        plt.text(0.165 + desp, 0.004, '-----------------', fontsize=5,color='coral', bbox=dict(facecolor='coral', edgecolor='coral', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.2 + desp, 0, 'Trastorno Mental', fontsize=14, transform=plt.gcf().transFigure);

        plt.text(0.34 + desp, 0.004, '-----------------', fontsize=5,color='firebrick', bbox=dict(facecolor='firebrick', edgecolor='firebrick', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.375 + desp, 0, 'Educación', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.47 + desp, 0.004, '-----------------', fontsize=5,color='saddlebrown', bbox=dict(facecolor='saddlebrown', edgecolor='saddlebrown', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.505 + desp, 0, 'Salud', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.57 + desp, 0.004, '-----------------', fontsize=5,color='cornflowerblue', bbox=dict(facecolor='cornflowerblue', edgecolor='cornflowerblue', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.605 + desp, 0, 'Política', fontsize=14, transform=plt.gcf().transFigure);


        plt.text(0.68 + desp, 0.004, '-----------------', fontsize=5,color='darkgray', bbox=dict(facecolor='darkgray', edgecolor='darkgray', boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.715 + desp, 0, 'Sin categoría', fontsize=14, transform=plt.gcf().transFigure);


def myplot(score, coeff, y, list_biplot, labels, index_pc, ax, mult, no_label):
    xs = score[:, index_pc[0]]
    ys = score[:, index_pc[1]]
    n = coeff.shape[0]
    scalex = 1.0/(xs.max() - xs.min())
    scaley = 1.0/(ys.max() - ys.min())

    colors = ['saddlebrown', 'maroon', 'coral', 'gold', 'cornflowerblue', 'orchid', 'wheat']
    
    #fig, ax = plt.subplots(figsize=(12,12))
    
    scatter = ax.scatter(xs * scalex,ys * scaley, c = list(y.values), cmap='Dark2',alpha = 0.5)
    c_count = 0
    for i in list_biplot:#range(n):
        ax.arrow(0, 0, coeff[i,0], coeff[i,1], color = colors[c_count],alpha = 0.8, lw = 4)
        c_count +=1

        if(no_label == False):
          if labels is None:
              ax.text(coeff[i,0]* 1.15, coeff[i,1] * 1.15, "Var"+str(i+1), color = 'g', ha = 'center', va = 'center')
          else:
              ax.text(coeff[i,0]* 1.15, coeff[i,1] * 1.15, labels[i], color = 'k', ha = 'center', va = 'center', fontsize=15, weight = 'bold')

    ax.set_xlabel("PC{}".format(index_pc[0] + 1))
    ax.set_ylabel("PC{}".format(index_pc[1] + 1))
    
    legend1 = ax.legend(handles=scatter.legend_elements()[0],
                        labels=['Sin violencia', 'Con violencia'],
                        loc="lower left", title="Clases")
    ax.add_artist(legend1)

    ax.text(0.025, 0.9, mult*'---'+'-----------------\n\n\n', horizontalalignment='left', verticalalignment='center', fontsize=15,color = 'w', bbox=dict(facecolor= 'w', edgecolor= 'w', boxstyle='round,pad=0.5'), transform=ax.transAxes)
    ax.text(0.025, 0.057, 5*'---'+'-----------------\n\n', horizontalalignment='left', verticalalignment='center', fontsize=12,color = 'w', bbox=dict(facecolor= 'w', edgecolor= 'w', boxstyle='round,pad=0.5'), transform=ax.transAxes)

    c_count = 0
    for i in list_biplot:

      ax.text(0.025, 0.95 - 0.025*c_count, '-----------------', horizontalalignment='left', verticalalignment='center', fontsize=4,color=colors[c_count], bbox=dict(facecolor=colors[c_count], edgecolor=colors[c_count], boxstyle='round,pad=0.5'), transform=ax.transAxes)
      ax.text(0.1 , 0.95 - 0.025*c_count, labels[i], horizontalalignment='left', weight="bold", verticalalignment='center', fontsize= 12,  transform=ax.transAxes);
      c_count +=1
    ax.grid()
    
    
    
def pca_classifier_ROC(X,y):
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=10000, stratify=y)

    cv    = RepeatedKFold(n_splits=5, n_repeats=100, random_state=101)
    folds = [(train,test) for train, test in cv.split(X_train, y_train)]
    metrics = ['auc', 'fpr', 'tpr', 'thresholds', 'f1_score', 'accuracy_score', 'recall_score', 'precision_score', 'confusion_matrix']
    results = {
        'train': {m:[] for m in metrics},
        'val'  : {m:[] for m in metrics},
        'test' : {m:[] for m in metrics}
    }
    X_train = pd.DataFrame(X_train)
    y_train = pd.DataFrame(y_train)


    params = {
        'objective'   : 'binary:logistic',
        'eval_metric' : 'logloss'
    }


    n = 4

    column_lists = []
    for i in range(1,n+1):
        column_lists.append('PC' + str(i))


    plt.rcParams["figure.figsize"] = (10,10)

    pca_pipes = make_pipeline(StandardScaler(), PCA(n_components=n))
    pca_pipes.fit(X_train)

    t_pcas = pca_pipes.transform(X=X_test)
    t_pcas = pd.DataFrame(t_pcas, columns = column_lists, index = X_test.index)

    dtest = xgb.DMatrix(t_pcas, label=y_test)

    for train, test in tqdm(folds, total=len(folds)):

        pca_pipes_roc = make_pipeline(StandardScaler(), PCA(n_components=n))
        pca_pipes_roc.fit(X_train.iloc[train,:])

        train_pcas = pca_pipes_roc.transform(X=X_train.iloc[train,:])
        train_pcas = pd.DataFrame(train_pcas, columns = column_lists, index = X_train.iloc[train,:].index)

        test_pcas  = pca_pipes_roc.transform(X=X_train.iloc[test,:])
        test_pcas  = pd.DataFrame(test_pcas, columns = column_lists, index = X_train.iloc[test,:].index)


        dtrain = xgb.DMatrix(train_pcas, label=y_train.iloc[train])
        dval   = xgb.DMatrix(test_pcas, label=y_train.iloc[test])
        model  = xgb.train(
            dtrain                = dtrain,
            params                = params, 
            evals                 = [(dtrain, 'train'), (dval, 'val')],
            num_boost_round       = 1000,
            verbose_eval          = False,
            early_stopping_rounds = 10,
        )
        sets = [dtrain, dval, dtest]

        for i,ds in enumerate(results.keys()):
            y_preds              = model.predict(sets[i])
            labels               = sets[i].get_label()
            fpr, tpr, thresholds = roc_curve(labels, y_preds)
            results[ds]['fpr'].append(fpr)
            results[ds]['tpr'].append(tpr)
            results[ds]['thresholds'].append(thresholds)
            results[ds]['auc'].append(roc_auc_score(labels, y_preds))

            results[ds]['f1_score'].append(f1_score(labels, np.round(y_preds)))
            results[ds]['accuracy_score'].append(accuracy_score(labels, np.round(y_preds)))
            results[ds]['recall_score'].append(recall_score(labels, np.round(y_preds)))
            results[ds]['precision_score'].append(precision_score(labels, np.round(y_preds)))
            results[ds]['confusion_matrix'].append(confusion_matrix(labels, np.round(y_preds)))


    kind = 'test'
    c_fill      = 'rgba(52, 152, 219, 0.2)'
    c_line      = 'rgba(52, 152, 219, 0.5)'
    c_line_main = 'rgba(41, 128, 185, 1.0)'
    c_grid      = 'rgba(189, 195, 199, 0.5)'
    c_annot     = 'rgba(149, 165, 166, 0.5)'
    c_highlight = 'rgba(192, 57, 43, 1.0)'

    fpr_mean    = np.linspace(0, 1, 100)
    interp_tprs = []

    for i in range(100):
        fpr           = results[kind]['fpr'][i]
        tpr           = results[kind]['tpr'][i]
        interp_tpr    = np.interp(fpr_mean, fpr, tpr)
        interp_tpr[0] = 0.0
        interp_tprs.append(interp_tpr)

    tpr_mean     = np.mean(interp_tprs, axis=0)
    tpr_mean[-1] = 1.0
    tpr_std      = 2*np.std(interp_tprs, axis=0)
    tpr_upper    = np.clip(tpr_mean+tpr_std, 0, 1)
    tpr_lower    = tpr_mean-tpr_std
    auc          = np.mean(results[kind]['auc'])

    fig = go.Figure([
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_upper,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'upper'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_lower,
            fill       = 'tonexty',
            fillcolor  = c_fill,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'lower'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_mean,
            line       = dict(color=c_line_main, width=2),
            hoverinfo  = "skip",
            showlegend = True,
            name       = f'AUC: {auc:.3f}')
    ])

    fig.add_shape(
        type ='line', 
        line =dict(dash='dash'),
        x0=0, x1=1, y0=0, y1=1
    )

    fig.update_layout(
        template    = 'plotly_white', 
        title_x     = 0.5,
        xaxis_title = "1 - Specificity",
        yaxis_title = "Sensitivity",
        width       = 800,
        height      = 800,
        legend      = dict(
            yanchor="bottom", 
            xanchor="right", 
            x=0.95,
            y=0.01,
        )
    )

    fig.update_yaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        scaleanchor = "x", 
        scaleratio  = 1,
        linecolor   = 'black')

    fig.update_xaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        constrain   = 'domain',
        linecolor   = 'black')
    
    fig.show()
    
    
    return (model, results)



def plot_class_report(model, results, kind):
    
    keys = ['f1_score', 'accuracy_score', 'recall_score', 'precision_score']


    result_metrics ={}

    for k in keys:
        result_metrics[k] = results[kind][k]

    result_metrics = pd.DataFrame(result_metrics)

    plt.figure(figsize=(8,8))
    sns.boxplot(data=result_metrics)

    plt.xlabel('Metrics')

    plt.show()
    
    
    all_confusion_matrix = np.zeros([2,2,len(results[kind]['confusion_matrix'])])

    count =0
    for i in range(len(results[kind]['confusion_matrix'])):
        all_confusion_matrix[:,:,count] = results[kind]['confusion_matrix'][count]
        count+=1

    mean_confusion_matrix = np.zeros([2,2])
    std_confusion_matrix = np.zeros([2,2])

    for i in range(2):
        for j in range(2):
            mean_confusion_matrix[i,j] = np.mean(all_confusion_matrix[i,j,:]) 
            std_confusion_matrix[i,j] = np.std(all_confusion_matrix[i,j,:])
        

    plt.figure(figsize=(5,5))
    plt.imshow(mean_confusion_matrix, cmap='Blues')
    plt.xlabel('Predicted Label')
    plt.ylabel('True Label')
    plt.grid()

    plt.xticks(np.arange(2), ['Sin Violencia', 'Con Violencia'])
    plt.yticks(np.arange(2), ['Sin Violencia', 'Con Violencia'], rotation=90)

    for (j,i),label in np.ndenumerate(mean_confusion_matrix):
          plt.text(i,j, str(np.int(mean_confusion_matrix[i,j])) + ' $\pm$ ('+ str(np.int(std_confusion_matrix[i,j]))+')',ha='center',va='center', color='k',fontsize=18)
    
    
    #plt.figure(figsize=(10,8))
    #plot_importance(model) 


def show_kmo_bartlett(X_scaler):
    print('Bartlett')
    print('\tstatistic:', np.round(ss.bartlett(*X_scaler)[0], 2), '\tpvalue:', ss.bartlett(*X_scaler)[1])
    
    kmo_all,kmo_model=calculate_kmo(X_scaler)
    print('\n')
    print('KMO')
    print('\tvalue:', np.round(kmo_model,2))
    
    
def get_color_for_EFA(list_EFA, file = 'headerv02.csv'):
    X_cat = pd.read_csv(file, encoding='latin-1', sep=";")
    X_cat_code =  list(X_cat['code'])
    X_cat_name =  list(X_cat['newcat'])
    
    X_cat_name_ = []
    for i in X_cat_name:
        X_cat_name_.append(i.split('_')[0])
        
    X_cat_name_ =  np.unique(X_cat_name_)
    
    color_dict = {}
    color_dict['Copy'] = [209/255, 107/255, 19/255]
    color_dict['Cult'] = [40/255, 43/255, 95/255]
    color_dict['Health'] = [237/255, 187/255, 3/255]
    color_dict['Memb'] = [49/255, 88/255, 165/255]
    color_dict['Pers'] = [231/255,31/255,114/255]
    color_dict['Pol'] = [52/255,170/255,225/255]
    color_dict['Psych'] = [224/255,49/255,19/255]
    color_dict['S_Ad'] = [119/255,191/255,141/255]
    color_dict['Soc'] = [65/255,146/255,53/255]
    color_dict['Violence_Committed'] = 'red'


    bar_color_total = []

    for i in list_EFA:
        bar_color_per_PC = []
        #for i in list(result_['df_PC_rep_list_I'][j].columns):
        index_code = X_cat_code.index(i)
        cat = X_cat_name[index_code]
        bar_color_total.append(color_dict[cat])
        
    return bar_color_total, color_dict

    
def plot_EFA_clusters(list_EFA, n_factors, bar_color, dict_color, xlim = 1800):
    
    
    colors = ['lawngreen', 'orange', 'coral', 'gold', 'cornflowerblue', 'orchid', 'wheat', 'darkgreen', 'bisque', 'olive', 'hotpink', 'greenyellow', 'khaki',
              'indianred', 'aqua', 'cornsilk', 'lawngreen', 'orange', 'coral', 'gold', 'cornflowerblue', 'orchid', 'wheat', 'darkgreen', 'bisque', 'olive', 'hotpink', 'greenyellow', 'khaki',
              'indianred', 'aqua', 'cornsilk']

    fig, ax = plt.subplots(figsize=(16,9))

    plt.xlim(-70, xlim)
    plt.ylim(-20, 300)

    star_j =[0, 22, 15, 32, 0, 22, 12, 32, 0, 22, 12, 32, 0, 22, 12, 32, 0, 22, 12, 32, 0, 22, 12, 32, 0, 22, 12, 32, 0, 22, 12, 32]

    count_i = 0
    count_i_leg = 0
    
    
    count_i_legend = 0
    for key in dict_color.keys():
        if(key == 'Mdisorder'):
            continue
        desp = 0.15

        plt.text(0.165 + desp + count_i_legend/34, 0.1, '---', fontsize=5,color = dict_color[key], bbox=dict(facecolor=dict_color[key], edgecolor=dict_color[key], boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.172 + desp + count_i_legend/34, 0.095, key , fontsize=10, transform=plt.gcf().transFigure);
        
        count_i_legend+=2

        
    
    count_color = 0
    #efa_len = len(list_EFA)
    for i in range(n_factors):
      #  if(i >= efa_len):
          #  continue
        
        if(len(list_EFA[i]) == 0):
            count_i += 1
            continue

      # Legenda manual
        #desp = -0.05

        #plt.text(0.165 + desp + count_i_leg/17, 0.1, '---', fontsize=5,color=colors[i], bbox=dict(facecolor=colors[i], edgecolor=colors[i], boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        #plt.text(0.172 + desp + count_i_leg/17, 0.095, 'Factor ' + str(i+1), fontsize=10, transform=plt.gcf().transFigure);

        count_j = 0

        if i % 2 == 0:
            k = 0
            count_i += 1
        else:
            k = 1
        count_i_leg +=1
        
        
        count = 0
        for j in range(star_j[i], star_j[i] + len(list_EFA[i])):
           # if(A[i,j]>0.52):
                #print(i,j)
            
            if(count == 0):
                star_box_j = j 
                #print(j)
            count+=1
            
            plt.text(count_i*250, j*7.5, 
                         str(list_EFA[i][count_j]),
                         size=11 , rotation=0.,
                         ha="center", va="center", color=bar_color[count_color]

                         );
            count_color+=1
            count_j+= 1
# add a rectangle
        ax.add_patch(Rectangle((count_i*250 - 230, 7.5*(star_box_j-0.7)), 450, 8*(count), edgecolor="black", facecolor='white'))
        plt.text(count_i*250 , 7.5*(j  + 1.5), 'Factor ' + str(i+1), ha="center", va="center")
        #plt.text(star_box*200, j*7, '---', fontsize=5,color=colors[i], bbox=dict(facecolor=colors[i], edgecolor=colors[i], boxstyle='round,pad=0.5'))

    plt.xticks([])
    plt.yticks([])
    plt.show()
    
    
def plot_and_compute_EFA(X, X_scaler, n_factors, plot = False):

    fa = FactorAnalyzer()
    fa.set_params(n_factors=n_factors, rotation='varimax')
    fa.fit(X_scaler)

    ev, v = fa.get_eigenvalues()


    ef = np.where(ev<=1)[0][0]
 
    
    if(plot):
        plt.figure(figsize=(10,6))
        plt.scatter(range(1,X.shape[1]+1),ev)
        plt.plot(range(1,X.shape[1]+1),ev)
        #plt.title('Scree Plot')
        plt.xlabel('Factors')
        plt.ylabel('Eigenvalue')
        plt.axhline(y=1,c='k');
        
        plt.annotate('Factor ' + str(ef), xy=(ef, 1), xytext=(ef+1, 5),
                    arrowprops=dict(facecolor='black', shrink=0.05), size=12,
                    ha="center", va="center",bbox=dict(boxstyle="round",
                           ec=(255/255, 102/255, 90/255),
                            fc=(255/255, 102/255, 90/255),
                          )
                    );
    
    
    Z=np.abs(fa.loadings_)

    col_name = []
    for i in range(1, n_factors+1):
        col_name.append('Factor ' + str(i))

    Z_pd = pd.DataFrame(data = Z, index = X.columns, columns = col_name)
    
    return Z_pd
    
    
    
def plot_EFA_threshold(Z_pd, thres):

    plt.figure(figsize=(12,6))
    plt.subplot(1,2,1)
    sns.heatmap(Z_pd, cmap="YlOrRd")
    plt.yticks([])
    plt.xticks([]);
    plt.ylabel('Features')
    plt.xlabel('Factors')
    plt.title('Loading EFA')

    plt.subplot(1,2,2)
    sns.heatmap(Z_pd[Z_pd > thres], cmap="YlOrRd")
    plt.yticks([]);
    plt.xticks([]);
    plt.ylabel('Features')
    plt.xlabel('Factors')
    plt.title('Loading EFA threshold > ' + str(thres));
    
    
def compute_or_load_SFS(df, X_scaler, y, list_EFA_feat, name_sfs):
    
    X_scaler_df = pd.DataFrame(X_scaler, columns = df.columns)
    X_scaler_EFA_df = X_scaler_df[list_EFA_feat]

    try:
        with open('SFS_Output/' + name_sfs + '.dat', 'rb') as file:
            sfs_ = pickle.load(file)
    except:

        lr_ = XGBClassifier()

        sfs_ = SFS(lr_, 
                  k_features="best", 
                  forward=True, 
                  floating=False, 
                  scoring='accuracy',
                  verbose=2,
                  cv=10,
                  n_jobs = -1)
        sfs_ = sfs_.fit(X_scaler_EFA_df, y)


        with open('SFS_Output/' + name_sfs + '.dat', 'wb') as file:
            pickle.dump(sfs_,file)

    return (X_scaler_EFA_df,sfs_)


def compute_EFA_df(df, X_scaler, list_EFA_feat):
    
 
    X_scaler_df = pd.DataFrame(X_scaler, columns = df.columns)
    X_scaler_EFA_df = X_scaler_df[list_EFA_feat]

    return X_scaler_EFA_df


def plot_sfs_(sfs, X_scaler_EFA_df):
    plt.rcParams["figure.figsize"] = (10,6)

    fig = plot_sfs(sfs.get_metric_dict(), kind='std_err')
    plt.xticks(rotation=90)
    plt.title('Sequential Forward Selection (w. StdErr)')
    plt.xticks(np.linspace(0,X_scaler_EFA_df.shape[1],15).astype(np.int), np.linspace(0,X_scaler_EFA_df.shape[1],15).astype(np.int))
    #plt.grid()
    plt.show()
    
    
def get_EFA_cluster(Z_pd, thres, n_factor):

    list_EFA = []
    dict_EFA= {}
    for i in range(n_factor):
        if(len(list(Z_pd[Z_pd.iloc[:,i] > thres].index))< 2):
            list_EFA.append([])
            continue
        list_EFA.append(list(Z_pd[Z_pd.iloc[:,i] > thres].index))
        dict_EFA[i] = list(Z_pd[Z_pd.iloc[:,i] > thres].index)
        
    list_EFA_feat = []
     
       
    #efa_len = len(list_EFA)
    for i in range(n_factor):
        #if(i >= efa_len):
           # continue
        for j in list_EFA[i]:
            list_EFA_feat.append(j)
        
    return (list_EFA, list_EFA_feat)



def classifier_ROC_sfs_EFA(X_scaler_EFA_df, y, sfs_):
    
    X_train, X_test, y_train, y_test = train_test_split(X_scaler_EFA_df, y, test_size=0.2, random_state=10000, stratify=y)

    lista = list(sfs_.k_feature_names_)

    X_train = X_train[lista]
    X_test = X_test[lista]

    print(X_train.shape)
    cv    = RepeatedKFold(n_splits=5, n_repeats=100, random_state=101)
    folds = [(train,test) for train, test in cv.split(X_train, y_train)]
    metrics = ['auc', 'fpr', 'tpr', 'thresholds', 'f1_score', 'accuracy_score', 'recall_score', 'precision_score', 'confusion_matrix']
    results = {
        'train': {m:[] for m in metrics},
        'val'  : {m:[] for m in metrics},
        'test' : {m:[] for m in metrics}
    }
    X_train = pd.DataFrame(X_train)
    y_train = pd.DataFrame(y_train)


    params = {
        'objective'   : 'binary:logistic',
        'eval_metric' : 'logloss'
    }


    plt.rcParams["figure.figsize"] = (10,10)
    dtest = xgb.DMatrix(X_test, label=y_test)
    for train, test in tqdm(folds, total=len(folds)):
        dtrain = xgb.DMatrix(X_train.iloc[train,:], label=y_train.iloc[train])
        dval   = xgb.DMatrix(X_train.iloc[test,:], label=y_train.iloc[test])
        model  = xgb.train(
            dtrain                = dtrain,
            params                = params, 
            evals                 = [(dtrain, 'train'), (dval, 'val')],
            num_boost_round       = 1000,
            verbose_eval          = False,
            early_stopping_rounds = 10,
        )
        sets = [dtrain, dval, dtest]

        for i,ds in enumerate(results.keys()):
            y_preds              = model.predict(sets[i])
            labels               = sets[i].get_label()
            fpr, tpr, thresholds = roc_curve(labels, y_preds)
            results[ds]['fpr'].append(fpr)
            results[ds]['tpr'].append(tpr)
            results[ds]['thresholds'].append(thresholds)
            results[ds]['auc'].append(roc_auc_score(labels, y_preds))

            results[ds]['f1_score'].append(f1_score(labels, np.round(y_preds)))
            results[ds]['accuracy_score'].append(accuracy_score(labels, np.round(y_preds)))
            results[ds]['recall_score'].append(recall_score(labels, np.round(y_preds)))
            results[ds]['precision_score'].append(precision_score(labels, np.round(y_preds)))
            results[ds]['confusion_matrix'].append(confusion_matrix(labels, np.round(y_preds)))


    kind = 'test'
    c_fill      = 'rgba(52, 152, 219, 0.2)'
    c_line      = 'rgba(52, 152, 219, 0.5)'
    c_line_main = 'rgba(41, 128, 185, 1.0)'
    c_grid      = 'rgba(189, 195, 199, 0.5)'
    c_annot     = 'rgba(149, 165, 166, 0.5)'
    c_highlight = 'rgba(192, 57, 43, 1.0)'

    fpr_mean    = np.linspace(0, 1, 100)
    interp_tprs = []

    for i in range(100):
        fpr           = results[kind]['fpr'][i]
        tpr           = results[kind]['tpr'][i]
        interp_tpr    = np.interp(fpr_mean, fpr, tpr)
        interp_tpr[0] = 0.0
        interp_tprs.append(interp_tpr)

    tpr_mean     = np.mean(interp_tprs, axis=0)
    tpr_mean[-1] = 1.0
    tpr_std      = 2*np.std(interp_tprs, axis=0)
    tpr_upper    = np.clip(tpr_mean+tpr_std, 0, 1)
    tpr_lower    = tpr_mean-tpr_std
    auc          = np.mean(results[kind]['auc'])

    fig = go.Figure([
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_upper,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'upper'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_lower,
            fill       = 'tonexty',
            fillcolor  = c_fill,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'lower'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_mean,
            line       = dict(color=c_line_main, width=2),
            hoverinfo  = "skip",
            showlegend = True,
            name       = f'AUC: {auc:.3f}')
    ])

    fig.add_shape(
        type ='line', 
        line =dict(dash='dash'),
        x0=0, x1=1, y0=0, y1=1
    )

    fig.update_layout(
        template    = 'plotly_white', 
        title_x     = 0.5,
        xaxis_title = "1 - Specificity",
        yaxis_title = "Sensitivity",
        width       = 800,
        height      = 800,
        legend      = dict(
            yanchor="bottom", 
            xanchor="right", 
            x=0.95,
            y=0.01,
        )
    )

    fig.update_yaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        scaleanchor = "x", 
        scaleratio  = 1,
        linecolor   = 'black')

    fig.update_xaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        constrain   = 'domain',
        linecolor   = 'black')



    fig.show()

    return (model, results)


def search_and_plot_best_factor(dict_result_by_factor, list_EFA, kind, xlim, size, plot_fact):    

    best_i = 0
    best_accuracy = 0

    for i in range(10):
        try:
            print(i, np.mean(dict_result_by_factor[i][1][kind]['accuracy_score']))
            acc = np.mean(dict_result_by_factor[i][1][kind]['accuracy_score'])

            if(acc > best_accuracy):
                best_accuracy= acc
                best_i = i
        except:
            pass

    print('\nBest factor index:',best_i)
    plot_importance_EFA(dict_result_by_factor[best_i][0], list_EFA[best_i], xlim, 12, False, [],best_i)


def compute_factor_iter(X_scaler_EFA_df, y, list_EFA):

    dict_result_by_factor = {}

    for i in range(len(list_EFA)):

        factor_list = list_EFA[i]

        if(len(factor_list) <= 0):
            continue

        dict_result_by_factor[i] = classifier_ROC_sfs_EFA_by_factor_iter(X_scaler_EFA_df, y, factor_list, i)
        
    return dict_result_by_factor


def plot_importance_EFA(model, list_EFA, xlim, size, plot_fact, file, dict_color, ind = 0):

    dict_factor = {} 
    for i in range(len(list_EFA)):
        if(len(list_EFA[i]) <=0):
            continue
        for var in list_EFA[i]:
            dict_factor[var] = i

    df = pd.DataFrame([[key, model.get_fscore()[key]] for key in model.get_fscore().keys()], columns= ['Features', 'f_score'])

    col = list(df.sort_values('f_score',ascending=False).iloc[:,0])
    bar_color,  dict_color = get_color_for_EFA(col, file)

    plt.figure(figsize=(6,8))
    sns.barplot(x="f_score", y="Features", data = df.sort_values('f_score', ascending=False), palette =bar_color)
    plt.xlim([0, xlim])
    
    
    if(plot_fact):
        y_step = 0
        for i in col:
            plt.text(5, y_step, 
                     'Factor: ' + str(dict_factor[i]+1),
                     size= size, rotation=0.,
                     ha="left", va="center", color = 'orange',
                     bbox=dict(boxstyle="round",
                               ec=(50/255, 50/255, 50/255),
                                fc=(50/255, 50/255, 50/255),
                               )
                     )
            y_step+=1
    else:
        plt.text(xlim - 30, len(list_EFA) - 0.75, 
            'Factor: ' + str(ind+1),
            size= size, rotation=0.,
            ha="left", va="center", color = 'orange',
            bbox=dict(boxstyle="round",
                ec=(50/255, 50/255, 50/255),
                fc=(50/255, 50/255, 50/255),
                    )
                 )

    count_i_legend = 0
    for key in dict_color.keys():
        if(key == 'Mdisorder'):
            continue
        desp = 0.025

        plt.text(0.165 + desp + count_i_legend/17, 0.025, '---', fontsize=5,color = dict_color[key], bbox=dict(facecolor=dict_color[key], edgecolor=dict_color[key], boxstyle='round,pad=0.5'), transform=plt.gcf().transFigure)
        plt.text(0.185 + desp + count_i_legend/17, 0.025, key , fontsize=10, transform=plt.gcf().transFigure);
        
        count_i_legend+=2.8
        
        
        
def classifier_ROC_EFA(X_scaler_EFA_df, y):
    
    X_train, X_test, y_train, y_test = train_test_split(X_scaler_EFA_df, y, test_size=0.2, random_state=10000, stratify=y)

    print(X_train.shape)
    
    cv    = RepeatedKFold(n_splits=5, n_repeats=100, random_state=101)
    folds = [(train,test) for train, test in cv.split(X_train, y_train)]
    metrics = ['auc', 'fpr', 'tpr', 'thresholds', 'f1_score', 'accuracy_score', 'recall_score', 'precision_score', 'confusion_matrix']
    results = {
        'train': {m:[] for m in metrics},
        'val'  : {m:[] for m in metrics},
        'test' : {m:[] for m in metrics}
    }
    X_train = pd.DataFrame(X_train)
    y_train = pd.DataFrame(y_train)


    params = {
        'objective'   : 'binary:logistic',
        'eval_metric' : 'logloss'
    }


    plt.rcParams["figure.figsize"] = (10,10)
    dtest = xgb.DMatrix(X_test, label=y_test)
    for train, test in tqdm(folds, total=len(folds)):
        dtrain = xgb.DMatrix(X_train.iloc[train,:], label=y_train.iloc[train])
        dval   = xgb.DMatrix(X_train.iloc[test,:], label=y_train.iloc[test])
        model  = xgb.train(
            dtrain                = dtrain,
            params                = params, 
            evals                 = [(dtrain, 'train'), (dval, 'val')],
            num_boost_round       = 1000,
            verbose_eval          = False,
            early_stopping_rounds = 10,
        )
        sets = [dtrain, dval, dtest]

        for i,ds in enumerate(results.keys()):
            y_preds              = model.predict(sets[i])
            labels               = sets[i].get_label()
            fpr, tpr, thresholds = roc_curve(labels, y_preds)
            results[ds]['fpr'].append(fpr)
            results[ds]['tpr'].append(tpr)
            results[ds]['thresholds'].append(thresholds)
            results[ds]['auc'].append(roc_auc_score(labels, y_preds))

            results[ds]['f1_score'].append(f1_score(labels, np.round(y_preds)))
            results[ds]['accuracy_score'].append(accuracy_score(labels, np.round(y_preds)))
            results[ds]['recall_score'].append(recall_score(labels, np.round(y_preds)))
            results[ds]['precision_score'].append(precision_score(labels, np.round(y_preds)))
            results[ds]['confusion_matrix'].append(confusion_matrix(labels, np.round(y_preds)))


    kind = 'test'
    c_fill      = 'rgba(52, 152, 219, 0.2)'
    c_line      = 'rgba(52, 152, 219, 0.5)'
    c_line_main = 'rgba(41, 128, 185, 1.0)'
    c_grid      = 'rgba(189, 195, 199, 0.5)'
    c_annot     = 'rgba(149, 165, 166, 0.5)'
    c_highlight = 'rgba(192, 57, 43, 1.0)'

    fpr_mean    = np.linspace(0, 1, 100)
    interp_tprs = []

    for i in range(100):
        fpr           = results[kind]['fpr'][i]
        tpr           = results[kind]['tpr'][i]
        interp_tpr    = np.interp(fpr_mean, fpr, tpr)
        interp_tpr[0] = 0.0
        interp_tprs.append(interp_tpr)

    tpr_mean     = np.mean(interp_tprs, axis=0)
    tpr_mean[-1] = 1.0
    tpr_std      = 2*np.std(interp_tprs, axis=0)
    tpr_upper    = np.clip(tpr_mean+tpr_std, 0, 1)
    tpr_lower    = tpr_mean-tpr_std
    auc          = np.mean(results[kind]['auc'])

    fig = go.Figure([
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_upper,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'upper'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_lower,
            fill       = 'tonexty',
            fillcolor  = c_fill,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'lower'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_mean,
            line       = dict(color=c_line_main, width=2),
            hoverinfo  = "skip",
            showlegend = True,
            name       = f'AUC: {auc:.3f}')
    ])

    fig.add_shape(
        type ='line', 
        line =dict(dash='dash'),
        x0=0, x1=1, y0=0, y1=1
    )

    fig.update_layout(
        template    = 'plotly_white', 
        title_x     = 0.5,
        xaxis_title = "1 - Specificity",
        yaxis_title = "Sensitivity",
        width       = 800,
        height      = 800,
        legend      = dict(
            yanchor="bottom", 
            xanchor="right", 
            x=0.95,
            y=0.01,
        )
    )

    fig.update_yaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        scaleanchor = "x", 
        scaleratio  = 1,
        linecolor   = 'black')

    fig.update_xaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        constrain   = 'domain',
        linecolor   = 'black')



    fig.show()

    return (model, results)




def classifier_ROC_sfs_EFA_by_factor_iter(X_scaler_EFA_df, y, factor_list, it_fact):
    
    X_train, X_test, y_train, y_test = train_test_split(X_scaler_EFA_df, y, test_size=0.2, random_state=10000, stratify=y)

    
    X_train = X_train[factor_list]
    X_test = X_test[factor_list]

    print('Factor: ', it_fact, X_train.shape)
    
    cv    = RepeatedKFold(n_splits=5, n_repeats=100, random_state=101)
    folds = [(train,test) for train, test in cv.split(X_train, y_train)]
    metrics = ['auc', 'fpr', 'tpr', 'thresholds', 'f1_score', 'accuracy_score', 'recall_score', 'precision_score', 'confusion_matrix']
    results = {
        'train': {m:[] for m in metrics},
        'val'  : {m:[] for m in metrics},
        'test' : {m:[] for m in metrics}
    }
    X_train = pd.DataFrame(X_train)
    y_train = pd.DataFrame(y_train)


    params = {
        'objective'   : 'binary:logistic',
        'eval_metric' : 'logloss'
    }


    plt.rcParams["figure.figsize"] = (10,10)
    dtest = xgb.DMatrix(X_test, label=y_test)
    for train, test in tqdm(folds, total=len(folds)):
        dtrain = xgb.DMatrix(X_train.iloc[train,:], label=y_train.iloc[train])
        dval   = xgb.DMatrix(X_train.iloc[test,:], label=y_train.iloc[test])
        model  = xgb.train(
            dtrain                = dtrain,
            params                = params, 
            evals                 = [(dtrain, 'train'), (dval, 'val')],
            num_boost_round       = 1000,
            verbose_eval          = False,
            early_stopping_rounds = 10,
        )
        sets = [dtrain, dval, dtest]

        for i,ds in enumerate(results.keys()):
            y_preds              = model.predict(sets[i])
            labels               = sets[i].get_label()
            fpr, tpr, thresholds = roc_curve(labels, y_preds)
            results[ds]['fpr'].append(fpr)
            results[ds]['tpr'].append(tpr)
            results[ds]['thresholds'].append(thresholds)
            results[ds]['auc'].append(roc_auc_score(labels, y_preds))

            results[ds]['f1_score'].append(f1_score(labels, np.round(y_preds)))
            results[ds]['accuracy_score'].append(accuracy_score(labels, np.round(y_preds)))
            results[ds]['recall_score'].append(recall_score(labels, np.round(y_preds)))
            results[ds]['precision_score'].append(precision_score(labels, np.round(y_preds)))
            results[ds]['confusion_matrix'].append(confusion_matrix(labels, np.round(y_preds)))


    kind = 'test'
    c_fill      = 'rgba(52, 152, 219, 0.2)'
    c_line      = 'rgba(52, 152, 219, 0.5)'
    c_line_main = 'rgba(41, 128, 185, 1.0)'
    c_grid      = 'rgba(189, 195, 199, 0.5)'
    c_annot     = 'rgba(149, 165, 166, 0.5)'
    c_highlight = 'rgba(192, 57, 43, 1.0)'

    fpr_mean    = np.linspace(0, 1, 100)
    interp_tprs = []

    for i in range(100):
        fpr           = results[kind]['fpr'][i]
        tpr           = results[kind]['tpr'][i]
        interp_tpr    = np.interp(fpr_mean, fpr, tpr)
        interp_tpr[0] = 0.0
        interp_tprs.append(interp_tpr)

    tpr_mean     = np.mean(interp_tprs, axis=0)
    tpr_mean[-1] = 1.0
    tpr_std      = 2*np.std(interp_tprs, axis=0)
    tpr_upper    = np.clip(tpr_mean+tpr_std, 0, 1)
    tpr_lower    = tpr_mean-tpr_std
    auc          = np.mean(results[kind]['auc'])

    fig = go.Figure([
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_upper,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'upper'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_lower,
            fill       = 'tonexty',
            fillcolor  = c_fill,
            line       = dict(color=c_line, width=1),
            hoverinfo  = "skip",
            showlegend = False,
            name       = 'lower'),
        go.Scatter(
            x          = fpr_mean,
            y          = tpr_mean,
            line       = dict(color=c_line_main, width=2),
            hoverinfo  = "skip",
            showlegend = True,
            name       = f'AUC: {auc:.3f}')
    ])

    fig.add_shape(
        type ='line', 
        line =dict(dash='dash'),
        x0=0, x1=1, y0=0, y1=1
    )

    fig.update_layout(
        template    = 'plotly_white', 
        title_x     = 0.5,
        xaxis_title = "1 - Specificity",
        yaxis_title = "Sensitivity",
        width       = 800,
        height      = 800,
        legend      = dict(
            yanchor="bottom", 
            xanchor="right", 
            x=0.95,
            y=0.01,
        )
    )

    fig.update_yaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        scaleanchor = "x", 
        scaleratio  = 1,
        linecolor   = 'black')

    fig.update_xaxes(
        range       = [0, 1],
        gridcolor   = c_grid,
        constrain   = 'domain',
        linecolor   = 'black')



    fig.show()

    return (model, results)