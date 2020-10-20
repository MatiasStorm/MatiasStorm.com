from django.shortcuts import render
from rest_framework import viewsets
from . import models, serializers

# Create your views here.
class PostCategoryViewSet(viewsets.ModelViewSet):
    queryset = models.PostCategory.objects.all()
    serializer_class = serializers.PostCategorySerializer

class SerieViewSet(viewsets.ModelViewSet):
    queryset = models.Serie.objects.all()
    serializer_class = serializers.SerieSerializer

class PostViewSet(viewsets.ModelViewSet):
    queryset = models.Post.objects.all()
    serializer_class = serializers.PostSerializer
