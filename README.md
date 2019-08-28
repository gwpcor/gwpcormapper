# Interactive Geographically Weighted Correlation Mapping

[WIP] This project holds the source code for an interactive mapping application
of geographically weighted correlation and partial correlation between data of
large datasets.

## Usage:

### Pre-requisites:
This program runs in a docker container, so all you need is to install
[docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/install/)
on your system.

### To Run:

1. Clone this repository.

```
git clone git@github.com:naru-T/gwpcormapper.git
```

If cloning a specific branch, like this
```
git clone -b BRANCH-NAME git@github.com:naru-T/gwpcormapper.git
```

2. Change directory to the project directory.

```
cd gwpcormapper
```


With [docker-compose](https://docs.docker.com/compose/install/) installed, simply run:

```
docker-compose up -d --b
```

5. Access the application at `http://localhost:80`

6. login name: rstudio / password: passwd

To stop the docker container, run: 

```
docker-compose down
```

## To contribute

If an issue doesnt exist, create one. Developments should be made on branches
from `develop` and merge requests made to develop. The `master` branch will
house all working code ready for preview and then later release. Thanks
:smile:
