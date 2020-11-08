clear all
close all
clc

%% Input initialisation.
%% Defining the probability distributions.
% Percentage probability of household size (including student)
%        2    3    4    5    6+
Hhsizes=[22.1 27.6 27.7 10.1 4.3]/100;
% Note we ignore "single student" families, as they can't infect anyone
% else.
% Data taken from Welsh Government analysis of the Annual Population Survey, 2019.
% ADD IN LINK.

% Rescale the probability boundaries to be 1.
Hhsizes=cumsum(Hhsizes/sum(Hhsizes));
Hhsizes=[0,Hhsizes];

% Mean probability (decimal) of infecting at least one other person in a
% household of size
%           2    3    4    5    6+
Mean_probs=[0.49 0.41 0.32 0.25 0.25];
% Standard deviation of probability distribution for a household of size
%   2      3      4      5      6+
Sd=[0.3617 0.3847 0.3535 0.2806 0.1255];
% Data from Tables 1 and 2 of "Transmission dynamics of COVID-19 in household and community
% settings in the United Kingdom" Lopez Bernal et al, 2020.
% https://doi.org/10.1101/2020.08.19.20177188

%% Simulation parameters
N=1000;                   % Number of students
NS=10000;                     % Number of simulations
I=[15 10 5 1.5 0.5]/100;	% Prevalence rates to be simulated


%% Monte-Carlo algorithm

for l=1:length(I)
    rate=I(l); % Run the code for each prevalence rate
    
    for j=1:NS % We average over NS simulations.
        
        % For each simulation, choose Nos uniformly randomly distributed numbers.
        % One for each simulated student. If a given random number is lower than
        % the prevalence then the simulated student is assumed to have
        % Covid-19. Thus, p1 is an indicator variable of whether each
        % student is infected.
        p1=rand(1,N)<rate;
        
        % Initialise p2 and p3.
        p2=[];
        p3=[];
        
        r=rand(1,N); % Generate Nos uniformly distributed random numbers to sample from the household distribution
        for i=1:length(Hhsizes)-1
            p2(i,:)=i*(Hhsizes(i)<=r).*(r<Hhsizes(i+1)); % Sampled household sizes, providing number of susceptible people.
            p3(i,:)=min(ones(1,N),abs(Mean_probs(i)+Sd(i)*randn(1,N))); % Probability of secondary infection.
        end
        
        p2p3=sum(p2.*p3); % Average number of infected inhibitants assuming additional infected occupant.
        
        Noi=p1.*p2p3; % Average number of secondary infections in each household.
        Noit(j)=sum(Noi); % Total number of secondary infections
    end
    
    %% Calculate the mean and standard deviations of the total number of secondary infections
    m(l)=mean(Noit);
    sd(l)=std(Noit);
end

%% Output results as a table
Prevalence_in_percent=100*I';
Number_of_Secondary_infection=round(m,1)';
Standard_deviation=round(sd,1)';
Confidence_interval_lower=max(0,Number_of_Secondary_infection-1.96*Standard_deviation/sqrt(NS));
Confidence_interval_upper=Number_of_Secondary_infection+1.96*Standard_deviation/sqrt(NS);


table(Prevalence_in_percent,Number_of_Secondary_infection,...
    Confidence_interval_lower,Confidence_interval_upper)


