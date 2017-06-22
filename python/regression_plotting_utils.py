import numpy as np                                        # a fundamental numerical linear algebra library
import matplotlib.pyplot as plt                           # a basic plotting library


# simple 2 panel plotting function - to show data and data + true function in separate panels
def cust_plt_util(data_x,data_y,true_x,true_y):

    # setup plot - left panel just data, right panel data + true func
    fig = plt.figure(figsize = (16,5))
    
    ## plot just data
    ax = fig.add_subplot(1,2,1)
    ax.scatter(data_x,data_y,facecolor = 'b',edgecolor = 'k',linewidth = 2.5)
    ax.set_xlim(min(data_x)-0.1,max(data_x)+0.1)
    ax.set_ylim(min(data_y)-0.1,max(data_y)+0.1)
    ax.set_yticks([],[])
    ax.axis('off') 

    ## plot data + true func
    ax = fig.add_subplot(1,2,2)
    ax.plot(true_x,true_y,'r--',linewidth = 2.5)
    ax.scatter(data_x,data_y,facecolor = 'b',edgecolor = 'k',linewidth = 2.5)
    ax.set_xlim(min(data_x)-0.1,max(data_x)+0.1)
    ax.set_ylim(min(data_y)-0.1,max(data_y)+0.1)
    ax.set_yticks([],[])
    ax.axis('off') 
    
# simple plotting function
def plot_example(data_x,data_y,true_x,true_y):
    # plot underlying data-generating function
    plt.plot(true_x,true_y,'r--',linewidth = 2.5)

    # plot data 
    plt.scatter(data_x,data_y,facecolor = 'b',edgecolor = 'k',linewidth = 2.5)

    # clean up the plot
    plt.xlim(min(data_x)-0.1,max(data_x)+0.1)
    plt.ylim(min(data_y)-0.1,max(data_y)+0.1)
    plt.yticks([],[])
    plt.axis('off') 
    
# plot approximation
def plot_approx(clf,data_x,data_y,true_x,true_y):
    # plot the data and true underlying function
    plot_example(data_x,data_y,true_x,true_y)
    
    # make input domain for plot
    s = np.linspace(min(data_x),max(data_x),300)[:, np.newaxis]
    
    # use regressor to make predictions across the input domain
    t = clf.predict(s)

    # plot regressor
    plt.plot(s,t,linewidth = 3,color = 'b')
    plt.ylim(min(min(data_y)-0.1,min(t)-0.1),max(max(data_y)+0.1,max(t)+0.1)) 