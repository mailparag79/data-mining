import numpy as np                                        # a fundamental numerical linear algebra library
import matplotlib.pyplot as plt                           # a basic plotting library

# simple 2 panel plotting function - to show data and data + true function in separate panels
def classify_cust_plt_util(data,labels,true_func):
    # define colors for plot
    colors = ['salmon','cornflowerblue','lime','bisque','mediumaquamarine','b','m','g']

    # separate coordinates of data and true function
    data_x = data[:,0]
    data_y = data[:,1]
    
    # setup plot - left panel just data, right panel data + true func
    fig = plt.figure(figsize = (8,4))
    ax1 = fig.add_subplot(1,2,1)
    ax1.set_xlim(min(data_x)-0.1,max(data_x)+0.1)
    ax1.set_ylim(min(data_y)-0.1,max(data_y)+0.1)
    ax1.set_yticks([],[])
    ax1.axis('off') 
    
    ax2 = fig.add_subplot(1,2,2)
    ax2.set_xlim(min(data_x)-0.1,max(data_x)+0.1)
    ax2.set_ylim(min(data_y)-0.1,max(data_y)+0.1)
    ax2.set_yticks([],[])
    ax2.axis('off') 
    
    # plot true function in right panel
    for i in range(int(np.shape(true_func)[1]*0.5)):
        true_x = true_func[:,2*i]
        true_y = true_func[:,2*i + 1]
        ax2.plot(true_x,true_y,color = 'k',linestyle = '--',linewidth = 4)

    # distinguish labels
    unique_labels = np.unique(labels)
    for i in range(len(unique_labels)):
        inds = np.argwhere(labels == unique_labels[i])
        inds = [s[0] for s in inds]

        # plot just data in left panel
        ax1.scatter(data_x[inds],data_y[inds],color = colors[i],linewidth = 1,marker = 'o',edgecolor = 'k',s = 60)

        # plot just data in right panel
        ax2.scatter(data_x[inds],data_y[inds],color = colors[i],linewidth = 1,marker = 'o',edgecolor = 'k',s = 60)

    
# function - plot data with underlying target function generated in the previous Python cell
def plot_data(data,labels,true_func):
    colors = ['salmon','cornflowerblue','lime','bisque','mediumaquamarine','b','m','g']

    # separate coordinates of data and true function
    data_x = data[:,0]
    data_y = data[:,1]

    # initialize figure
    fig = plt.figure(figsize = (4,4))
    ax = fig.add_subplot(1,1,1)
    ax.set_xlim(min(data_x)-0.1,max(data_x)+0.1)
    ax.set_ylim(min(data_y)-0.1,max(data_y)+0.1)
    ax.set_yticks([],[])
    ax.axis('off') 
    
    # plot true function in panel
    for i in range(int(np.shape(true_func)[1]*0.5)):
        true_x = true_func[:,2*i]
        true_y = true_func[:,2*i + 1]
        ax.plot(true_x,true_y,color = 'k',linestyle = '--',linewidth = 4)
    
    # plot data in panel
    unique_labels = np.unique(labels)
    for i in range(len(unique_labels)):
        inds = np.argwhere(labels == unique_labels[i])
        inds = [s[0] for s in inds]

        # plot just data in left panel
        ax.scatter(data_x[inds],data_y[inds],color = colors[i],linewidth = 1,marker = 'o',edgecolor = 'k',s = 60)
      
    
# plot approximation
def plot_approx(clf,data,labels,true_func):
    colors = ['salmon','cornflowerblue','lime','bisque','mediumaquamarine','b','m','g']
  
    # plot data first
    plot_data(data,labels,true_func)
    
    # plot classification boundary and color regions appropriately
    lower_bound = min(data.ravel())
    upper_bound = max(data.ravel())
    r = np.linspace(lower_bound - 0.1,upper_bound + 0.1,700)
    s,t = np.meshgrid(r,r)
    s = np.reshape(s,(np.size(s),1))
    t = np.reshape(t,(np.size(t),1))
    h = np.concatenate((s,t),1)

    # use classifier to make predictions
    z = clf.predict(h)

    # reshape predictions for plotting
    s.shape = (np.size(r),np.size(r))
    t.shape = (np.size(r),np.size(r))
    z.shape = (np.size(r),np.size(r))
  
    unique_labels = np.unique(labels)
    levels = unique_labels
    if len(levels) > 2:
        
        plt.contourf(s,t,z,colors = colors[0:len(unique_labels)+1],levels = range(0,len(unique_labels)+1), alpha = 0.2)
    else:
        plt.contourf(s,t,z,colors = colors[0:len(unique_labels)+1],alpha = 0.2) #levels = range(0,len(unique_labels)+1), alpha = 0.2)

    
    # show the classification boundary if it exists
    if len(np.unique(z)) > 1:
        plt.contour(s,t,z,colors = 'k',linewidths = 2.5, levels = unique_labels)

