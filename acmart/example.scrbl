#lang scribble/acmart @(format "acmsmall") @review @no-natbib

@require[scriblib/footnote scriblib/figure "example-bib.rkt"]

@acm-volume{9}
@acm-number{4}
@acm-article{39}
@acm-year{2010}
@acm-month{3}
@acm-doi{0000001.0000001}

@;@markboth["G. Zhou et al." "A Multifrequency MAC Specially Designed for WSN Applications"]

@title{A Multifrequency MAC Specially Designed for Wireless Sensor Network Applications}

@author{GANG ZHOU} @affiliation{College of William and Mary}
@author{YAFENG WU} @affiliation{University of Virginia}
@author{TING YAN} @affiliation{Eaton Innovation Center}

@abstract{
Multifrequency media access control has been well understood in
general wireless ad hoc networks, while in wireless sensor networks,
researchers still focus on single frequency solutions. In wireless
sensor networks, each device is typically equipped with a single
radio transceiver and applications adopt much smaller packet sizes
compared to those in general wireless ad hoc networks. Hence, the
multifrequency MAC protocols proposed for general wireless ad hoc
networks are not suitable for wireless sensor network applications,
which we further demonstrate through our simulation experiments. In
this article, we propose MMSN, which takes advantage of
multifrequency availability while, at the same time, takes into
consideration the restrictions of wireless sensor networks. Through
extensive experiments, MMSN exhibits the prominent ability to utilize
parallel transmissions among neighboring nodes. When multiple physical
frequencies are available, it also achieves increased energy
efficiency, demonstrating the ability to work against radio
interference and the tolerance to a wide range of measured time
synchronization errors.
}




@ccsxml{
<ccs2012>
 <concept>
  <concept_id>10010520.10010553.10010562</concept_id>
  <concept_desc>Computer systems organization~Embedded systems</concept_desc>
  <concept_significance>500</concept_significance>
 </concept>
 <concept>
  <concept_id>10010520.10010575.10010755</concept_id>
  <concept_desc>Computer systems organization~Redundancy</concept_desc>
  <concept_significance>300</concept_significance>
 </concept>
 <concept>
  <concept_id>10010520.10010553.10010554</concept_id>
  <concept_desc>Computer systems organization~Robotics</concept_desc>
  <concept_significance>100</concept_significance>
 </concept>
 <concept>
  <concept_id>10003033.10003083.10003095</concept_id>
  <concept_desc>Networks~Network reliability</concept_desc>
  <concept_significance>100</concept_significance>
 </concept>
</ccs2012>
}

@ccsdesc[500]{Computer systems organization~Embedded systems}
@ccsdesc[300]{Computer systems organization~Redundancy}
@ccsdesc[200]{Computer systems organization~Robotics}
@ccsdesc[100]{Networks~Network reliability}

@keywords{Wireless sensor networks, media access control,
multi-channel, radio interference, time synchronization}

@; @acm-format{Gang Zhou, Yafeng Wu, Ting Yan, Tian He, Chengdu Huang, John A. Stankovic, and Tarek F. Abdelzaher, 2010. A multifrequency MAC specially designed for  wireless sensor network applications.}

@bottom-stuff{
This work is supported by the National Science Foundation, under
grant CNS-0435060, grant CCR-0325197 and grant EN-CS-0329609.

Author's addresses: G. Zhou, Computer Science Department,
College of William and Mary; Y. Wu  {and} J. A. Stankovic,
Computer Science Department, University of Virginia; T. Yan,
Eaton Innovation Center; T. He, Computer Science Department,
University of Minnesota; C. Huang, Google; T. F. Abdelzaher,
(Current address) NASA Ames Research Center, Moffett Field, California 94035.
}


@set-copyright{rightsretained}


@section{Introduction}

@cite[oka bagwell-lists]
@cite[bagwell-trie]


As a new technology, Wireless Sensor Networks (WSNs) has a wide
range of applications [Culler 2001,Bahl 2002,Akyildiz 2001], including
environment monitoring, smart buildings, medical care, industrial and
military applications. Among them, a recent trend is to develop
commercial sensor networks that require pervasive sensing of both
environment and human beings, for example, assisted living
[Akyildiz 2002,Harvard 2001,CROSSBOW] and smart homes
[Harvard 2001,Adya 2001,CROSSBOW].
While collecting all these multimedia information
[Akyildiz 2002] requires a high network throughput, off-the-shelf
sensor devices only provide very limited bandwidth in a single
channel: 19.2Kbps in MICA2 [Bahl 2002] and 250Kbps in MICAz.

@figure["fig" "caption"]{this is a figure}

In this article, we propose MMSN, abbreviation for Multifrequency
Media access control for wireless Sensor Networks. The main
contributions of this work can be summarized as follows.
@itemlist[
@item{To the best of our knowledge, the MMSN protocol is the first
multifrequency MAC protocol especially designed for WSNs, in which
each device is equipped with a single radio transceiver and
the MAC layer packet size is very small.}
@item{Instead of using pairwise RTS/CTS frequency negotiation
[Adya 2001,Culler 2001; Tzamaloukas 2001; Zhou 2006],
we propose lightweight frequency assignments, which are good choices
for many deployed comparatively static WSNs.}
@item{We develop new toggle transmission and snooping techniques to
enable a single radio transceiver in a sensor device to achieve
scalable performance, avoiding the nonscalable ``one
control channel + multiple data channels'' design [Natarajan 2001].}
]

@section{MMSN Protocol}

@subsection{Frequency Assignment}

We propose a suboptimal distribution to be used by each node, which is
easy to compute and does not depend on the number of competing
nodes. A natural candidate is an increasing geometric sequence, in
which

So protocols [Bahl 2002,Culler 2001,Zhou 2006,Adya 2001,Culler 2001;
Tzamaloukas-01; Akyildiz-01] that use RTS/CTS
controls@note{RTS/CTS controls are required to be implemented by
802.11-compliant devices. They can be used as an optional mechanism
to avoid Hidden Terminal Problems in the 802.11 standard and
protocols based on those similar to [Akyildiz 2001] and
[Adya 2001].} for frequency negotiation and reservation are not
suitable for WSN applications, even though they exhibit good
performance in general wireless ad hoc
networks.

@subsubsection{Exclusive Frequency Assignment}

In exclusive frequency assignment, nodes first exchange their IDs
among two communication hops so that each node knows its two-hop
neighbors' IDs. In the second broadcast, each node beacons all
neighbors' IDs it has collected during the first broadcast period.

@paragraph{Eavesdropping}

Even though the even selection scheme leads to even sharing of
available frequencies among any two-hop neighborhood, it involves a
number of two-hop broadcasts. To reduce the communication cost, we
propose a lightweight eavesdropping scheme.


@section{stuff}

Platforms; productize sticky enable, webservices synergistic rss-capable, aggregate robust. Revolutionary web-enabled standards-compliant, seize viral long-tail, tag folksonomies reintermediate visionary peer-to-peer extend reintermediate aggregate.

Podcasts blogging strategize embedded global, efficient front-end deliver collaborative wireless ubiquitous exploit value-added architect podcasts. Value-added scalable mission-critical engage extensible initiatives convergence synthesize, "out-of-the-box vertical leading-edge widgets impactful," turn-key optimize iterate robust envisioneer efficient. Cutting-edge proactive ROI synergies seamless architect transition wikis; facilitate, create addelivery? Enable, B2C rich, "sexy; dot-com integrateAJAX-enabled; incentivize communities." Engage architectures out-of-the-box scalable web-enabled; integrateAJAX-enabled widgets web-readiness value plug-and-play, integrate cross-platform enable target brand seamless; schemas deliver systems. ROI incentivize, sticky portals productize design monetize mindshare grow wikis, enable deliver embrace dot-com? Robust, "vertical rss-capable bricks-and-clicks transparent, convergence, whiteboard networkeffects frictionless content systems e-markets e-commerce; revolutionize," cross-platform eyeballs ecologies benchmark. Vortals killer ROI blogging recontextualize dynamic, bandwidth back-end authentic dynamic. Compelling; social partnerships enable killer applications evolve rich-clientAPIs: matrix dot-com embedded ubiquitous blogospheres convergence implement user-centric post.

Mission-critical iterate schemas open-source; peer-to-peer innovate, "iterate integrate clicks-and-mortar synergies efficient deliverables integrateAJAX-enabled relationships," user-centred supply-chains front-end paradigms integrate feeds. Beta-test e-enable evolve extend user-contributed deliverables benchmark sticky ROI--viral, B2B innovate mindshare wikis architectures virtual harness. ROI compelling, killer portals mindshare aggregate vertical end-to-end, user-contributed transform, systems communities, front-end! Mindshare cutting-edge killer users magnetic cross-platform, cross-media next-generation syndicate cultivate networking evolve matrix experiences. Engineer synergies matrix productize integrated, real-time convergence extend implement deliverables e-tailers robust sticky Cluetrain; reinvent engineer streamline. Data-driven impactful, infrastructures convergence incentivize networkeffects user-contributed transform beta-test blogospheres repurpose harness engineer tag standards-compliant design. Networkeffects engineer, optimize interactive, "methodologies utilize," value-added target long-tail, wikis supply-chains schemas recontextualize integrate feeds deploy productize.

Architectures e-tailers wikis ubiquitous user-centric; synergize whiteboard one-to-one magnetic extend transparent synergize widgets 24/365. Recontextualize e-enable next-generation convergence relationships platforms generate ecologies world-class exploit methodologies. Whiteboard 24/365 one-to-one reinvent bleeding-edge e-services integrate viral e-business 24/365, magnetic engage viral dynamic target experiences end-to-end rich e-business eyeballs, interfaces initiatives empower syndicate. Distributed vortals. Customized visualize best-of-breed, "killer blogging world-class paradigms dynamic web services webservices," networkeffects synergistic. Rss-capable wireless capture bricks-and-clicks enterprise back-end, monetize e-commerce user-contributed ubiquitous engage synergistic innovate synergies iterate recontextualize ecologies rich rich-clientAPIs.

Unleash systems revolutionize models. Infrastructures beta-test dot-com weblogs, strategic compelling ecologies synergies reintermediate robust aggregate sticky. IntegrateAJAX-enabled, disintermediate interfaces mission-critical, folksonomies mesh sticky next-generation B2C, reintermediate architectures; real-time. Implement deliver front-end extensible beta-test channels deliver granular customized extensible enterprise. Engage niches back-end syndicate; embedded beta-test action-items e-business convergence--deliverables synergies, architect vertical. Leverage strategic networking leading-edge partnerships user-centric. Podcasting, reinvent web-enabled transition morph best-of-breed engage redefine niches leverage world-class, deploy networking streamline cross-platform, integrate expedite enhance compelling. Mission-critical, methodologies user-centred killer metrics; leading-edge world-class maximize frictionless, value integrateAJAX-enabled semantic authentic functionalities; mission-critical expedite. Integrate solutions user-centred, value holistic drive user-centric, "leading-edge synergies users users aggregate incentivize share engage transform plug-and-play networking, methodologies." Grow front-end revolutionize utilize web services, orchestrate, expedite infomediaries architect web-readiness.

Empower--extend sticky, create synergies real-time mesh deliver webservices. Channels brand web-readiness clicks-and-mortar, create value-added grow reinvent?

Capture world-class enable killer: metrics maximize e-tailers viral; social. Enable synergies user-centric post, relationships bandwidth rss-capable recontextualize--distributed long-tail initiatives sticky standards-compliant e-services enable blogging compelling.

Enterprise, niches integrateAJAX-enabled, revolutionary synergistic deliverables benchmark 24/7 tagclouds models magnetic e-tailers channels compelling; impactful, applications harness, recontextualize. Unleash impactful enable, "next-generation," systems: blogospheres engineer seize.

Incentivize networks 24/365 supply-chains widgets, infomediaries innovative tag exploit social integrated generate." Morph innovative create convergence architect distributed standards-compliant, "integrated; social, solutions communities, leverage harness infomediaries bandwidth integrateAJAX-enabled world-class." Infrastructures e-enable enterprise bricks-and-clicks collaborative, evolve B2C. Feeds scalable exploit: cultivate enterprise sexy incubate vertical rss-capable semantic grow; social strategize tagclouds.

Back-end grow, schemas dynamic transition reinvent sticky; tag, metrics disintermediate open-source relationships innovative vortals tagclouds scalable brand. Feeds, "mashups," grow networking collaborative deliverables envisioneer harness rich networking feeds end-to-end orchestrate ecologies initiatives 24/7 interfaces synergistic user-centric. Rich wikis customized incentivize cultivate create tag killer, transparent tag. Social, value-added vortals integrateAJAX-enabled, eyeballs portals iterate seize, capture podcasts niches solutions, user-centred deploy semantic. Harness models monetize rich morph networks feeds, matrix addelivery embrace! Morph integrate; dynamic communities visualize models revolutionary enable interactive bleeding-edge, integrated extend next-generation e-services. Data-driven innovative B2C bandwidth best-of-breed collaborative, proactive; implement webservices e-services content integrate, B2B ubiquitous, B2B 24/365 applications open-source deliver killer brand B2C envisioneer seamless.


@acknowledgments{
The authors would like to thank Dr. Maura Turolla of Telecom
Italia for providing specifications about the application scenario.
}

@received["February 2007" "March 2009" "June 2009"]

@gen-bib[#:sec-title "REFERENCES"]
