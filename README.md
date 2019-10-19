# gwpcorMapper
__a web-based tool for interactive geographically weighted correlation mapping__

[WIP] This project holds the source code for an interactive mapping application
of geographically weighted correlation and partial correlation between data of
large datasets.

## Usage:

### Pre-requisites:
This program runs in a docker container, so all you need is to install
[docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/install/)
on your system.

### Quick start:

1. Pull the docker containers from Docker hub (in a terminal):

```bash 
docker pull iosefa/gwpcormapper
```

2. Start the container: 
```bash
docker run -p 80:3838 iosefa/gwpcormapper
```

Now, open a web browser and go to [localhost](http://localhost:80) and you are all set!

To shut down the container:
```bash
docker stop iosefa/gwpcormapper
```

### Documentation:
[WIP] Documentation will be added shortly. Please be patient, or feel free to help out!

## Development version:

If you would like to use the latest development version:
1. clone this repository
2. switch to the `develop` branch.
3. in the repository directory, run: 

```bash
docker-compose up -d
```
to start the program, and:
```bash
docker-compose down
```
to shut down the program.


## To contribute

If an issue doesnt exist, create one! Fork the repository and send us a pull request. Thank you!
